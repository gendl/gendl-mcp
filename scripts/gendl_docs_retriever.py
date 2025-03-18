#!/usr/bin/env python3
# gendl_docs_retriever.py

import os
import json
import re
import argparse
from collections import Counter
import math
import logging

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

def parse_arguments():
    parser = argparse.ArgumentParser(description='Retrieve relevant Gendl documentation for a query')
    parser.add_argument('--knowledge-base', type=str, default='/projects/xfer/gendl_knowledge_base', 
                        help='Directory containing the processed knowledge base')
    parser.add_argument('--query', type=str, help='Query to search for')
    parser.add_argument('--interactive', action='store_true', help='Run in interactive mode')
    parser.add_argument('--max-results', type=int, default=5, help='Maximum number of results to return')
    parser.add_argument('--output', type=str, help='Output file for Claude context (optional)')
    return parser.parse_args()

def load_knowledge_base(kb_dir):
    """Load the knowledge base files"""
    docs_file = os.path.join(kb_dir, "all_documents.json")
    index_file = os.path.join(kb_dir, "inverted_index.json")
    
    if not os.path.exists(docs_file) or not os.path.exists(index_file):
        raise FileNotFoundError(f"Knowledge base files not found in {kb_dir}")
    
    logger.info(f"Loading documents from {docs_file}")
    with open(docs_file, 'r', encoding='utf-8') as f:
        documents = json.load(f)
    
    logger.info(f"Loading inverted index from {index_file}")
    with open(index_file, 'r', encoding='utf-8') as f:
        inverted_index = json.load(f)
    
    # Create document lookup by ID
    doc_lookup = {doc["id"]: doc for doc in documents}
    
    logger.info(f"Loaded {len(documents)} documents with {len(inverted_index)} indexed terms")
    return documents, inverted_index, doc_lookup

def tokenize_query(query):
    """Simple tokenization of the query"""
    # Convert to lowercase and extract words
    words = re.findall(r'\b\w+\b', query.lower())
    
    # Remove very common words
    stopwords = {'the', 'a', 'an', 'and', 'or', 'in', 'on', 'at', 'to', 'for', 'with', 'of', 'is', 'are'}
    return [word for word in words if word not in stopwords and len(word) > 2]

def search(query, inverted_index, doc_lookup, max_results=5):
    """Search for documents matching the query"""
    query_tokens = tokenize_query(query)
    
    if not query_tokens:
        return []
    
    logger.info(f"Searching for tokens: {query_tokens}")
    
    # Find documents containing the query terms
    matching_doc_ids = []
    
    # First try exact matches
    for token in query_tokens:
        if token in inverted_index:
            matching_doc_ids.extend(inverted_index[token])
    
    # If we don't have enough results, try partial matches
    if len(set(matching_doc_ids)) < max_results:
        for token in query_tokens:
            for term in inverted_index:
                if token in term and term != token:  # Avoid double-counting exact matches
                    matching_doc_ids.extend(inverted_index[term])
    
    # If still no results or only few results, try to find related terms
    # This simulates a bit of "fuzzy matching"
    if len(set(matching_doc_ids)) < max_results:
        for token in query_tokens:
            # Find terms that start with the same 3 letters
            if len(token) >= 3:
                prefix = token[:3]
                for term in inverted_index:
                    if term.startswith(prefix) and term != token:
                        matching_doc_ids.extend(inverted_index[term])
    
    # If still no matches, return empty result
    if not matching_doc_ids:
        logger.warning(f"No matches found for query: {query}")
        return []
    
    # Count occurrences to rank by relevance
    doc_counts = Counter(matching_doc_ids)
    
    # Get the most relevant documents
    top_doc_ids = [doc_id for doc_id, _ in doc_counts.most_common(max_results)]
    
    # Retrieve the actual documents
    results = []
    for doc_id in top_doc_ids:
        if doc_id in doc_lookup:
            results.append(doc_lookup[doc_id])
    
    logger.info(f"Found {len(results)} matching documents")
    return results

def format_results_for_claude(results, query):
    """Format search results for Claude's context"""
    formatted_text = f"# Gendl Documentation Search Results\n\nQuery: {query}\n\n"
    
    for i, doc in enumerate(results, 1):
        formatted_text += f"## Result {i}: {doc['metadata']['file_name']}\n"
        formatted_text += f"Source: {doc['metadata']['source']}\n"
        formatted_text += f"Type: {doc['metadata']['content_type']}\n"
        formatted_text += f"Topic: {doc['metadata']['topic']}\n\n"
        formatted_text += "```\n"
        formatted_text += doc['text']
        formatted_text += "\n```\n\n---\n\n"
    
    return formatted_text

def interactive_mode(kb_dir, max_results=5):
    """Run the retriever in interactive mode"""
    print(f"Loading knowledge base from {kb_dir}...")
    documents, inverted_index, doc_lookup = load_knowledge_base(kb_dir)
    
    print("\nGendl Documentation Retriever")
    print("-----------------------------")
    print("Type your query or 'quit' to exit.")
    
    while True:
        query = input("\nQuery: ").strip()
        
        if query.lower() in ('quit', 'exit', 'q'):
            break
        
        if not query:
            continue
        
        results = search(query, inverted_index, doc_lookup, max_results)
        
        if not results:
            print("No results found for your query.")
            continue
        
        print(f"\nFound {len(results)} relevant documents:")
        
        for i, doc in enumerate(results, 1):
            print(f"\n--- Result {i} ---")
            print(f"Source: {doc['metadata']['source']}")
            print(f"Type: {doc['metadata']['content_type']}")
            print(f"Topic: {doc['metadata']['topic']}")
            
            # Show a short preview
            preview = doc['text'][:250] + "..." if len(doc['text']) > 250 else doc['text']
            print(f"\nPreview: {preview}")
        
        # Ask if the user wants to see full content
        show_full = input("\nView full content of a result? (Enter number or 'n'): ").strip()
        
        if show_full.lower() != 'n' and show_full.isdigit():
            idx = int(show_full) - 1
            if 0 <= idx < len(results):
                print("\n" + "=" * 80)
                print(f"Full content of Result {idx+1}:")
                print("=" * 80)
                print(results[idx]['text'])
                print("=" * 80)

def main():
    args = parse_arguments()
    
    if args.interactive:
        interactive_mode(args.knowledge_base, args.max_results)
    elif args.query:
        documents, inverted_index, doc_lookup = load_knowledge_base(args.knowledge_base)
        results = search(args.query, inverted_index, doc_lookup, args.max_results)
        
        if not results:
            print("No results found for your query.")
            return
        
        formatted_results = format_results_for_claude(results, args.query)
        
        if args.output:
            with open(args.output, 'w', encoding='utf-8') as f:
                f.write(formatted_results)
            print(f"Results saved to {args.output}")
        else:
            print(formatted_results)
    else:
        print("Please provide a query with --query or use --interactive mode.")

if __name__ == "__main__":
    main()
