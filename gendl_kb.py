#!/usr/bin/env python3
"""
MCP function to query the Gendl knowledge base.
"""

import os
import sys
import json
import subprocess
from pathlib import Path

def query_gendl_kb(query):
    """Query the Gendl knowledge base with the given query."""
    kb_path = "/projects/xfer/gendl-mcp/gendl_knowledge_base"
    
    # First check if the knowledge base exists
    if not os.path.exists(kb_path):
        return "Error: Gendl knowledge base not found at /projects/xfer/gendl-mcp/gendl_knowledge_base"
    
    # Check if all_documents.json exists
    docs_path = os.path.join(kb_path, "all_documents.json")
    if not os.path.exists(docs_path):
        return "Error: Gendl knowledge base files not found. Please run the processor first."
    
    try:
        # Load documents and inverted index directly
        with open(docs_path, 'r', encoding='utf-8') as f:
            documents = json.load(f)
        
        index_path = os.path.join(kb_path, "inverted_index.json")
        with open(index_path, 'r', encoding='utf-8') as f:
            inverted_index = json.load(f)
        
        # Create document lookup by ID
        doc_lookup = {doc["id"]: doc for doc in documents}
        
        # Simple search function
        def search(query, inverted_index, doc_lookup, max_results=5):
            # Tokenize the query
            import re
            words = re.findall(r'\b\w+\b', query.lower())
            stopwords = {'the', 'a', 'an', 'and', 'or', 'in', 'on', 'at', 'to', 'for', 'with', 'of', 'is', 'are'}
            query_tokens = [word for word in words if word not in stopwords and len(word) > 2]
            
            if not query_tokens:
                return []
            
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
                        if token in term and term != token:
                            matching_doc_ids.extend(inverted_index[term])
            
            # If still no matches, return empty result
            if not matching_doc_ids:
                return []
            
            # Count occurrences to rank by relevance
            from collections import Counter
            doc_counts = Counter(matching_doc_ids)
            
            # Get the most relevant documents
            top_doc_ids = [doc_id for doc_id, _ in doc_counts.most_common(max_results)]
            
            # Retrieve the actual documents
            results = []
            for doc_id in top_doc_ids:
                if doc_id in doc_lookup:
                    results.append(doc_lookup[doc_id])
            
            return results
        
        # Format the results
        def format_results(results, query):
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
        
        # Search for results
        results = search(query, inverted_index, doc_lookup)
        
        if not results:
            return "No relevant information found in the Gendl knowledge base for this query."
        
        # Format the results
        return format_results(results, query)
        
    except Exception as e:
        import traceback
        return f"Error querying Gendl knowledge base: {str(e)}\n{traceback.format_exc()}"

if __name__ == "__main__":
    # If run directly, parse query from command line
    if len(sys.argv) > 1:
        query = sys.argv[1]
        result = query_gendl_kb(query)
        print(result)
    else:
        print("Please provide a query as an argument")
