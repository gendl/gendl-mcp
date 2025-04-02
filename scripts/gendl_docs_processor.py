#!/usr/bin/env python3
# gendl_docs_processor.py

import os
import re
import json
import glob
import argparse
import hashlib
from pathlib import Path
from datetime import datetime
import logging

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

def parse_arguments():
    # Get the directory where the script is located
    script_dir = Path(__file__).parent

    parser = argparse.ArgumentParser(description='Process Gendl documentation into searchable text')
    parser.add_argument('--input', type=str, default=str(script_dir / '../gendl_kb_data'),
                      help='Input directory containing Gendl documentation')
    parser.add_argument('--output', type=str, default=str(script_dir / '../gendl_kb'),
                      help='Output directory for processed documentation')
    parser.add_argument('--chunk-size', type=int, default=1000,
                      help='Maximum size of text chunks')
    parser.add_argument('--chunk-overlap', type=int, default=200,
                      help='Overlap between text chunks')
    return parser.parse_args()

def extract_text_from_html(html_content):
    """Extract meaningful text from HTML content"""
    try:
        from bs4 import BeautifulSoup
        soup = BeautifulSoup(html_content, 'html.parser')
        
        # Remove script and style elements
        for script in soup(["script", "style"]):
            script.extract()
        
        # Get text
        text = soup.get_text(separator=' ', strip=True)
        
        # Clean up whitespace
        lines = (line.strip() for line in text.splitlines())
        chunks = (phrase.strip() for line in lines for phrase in line.split("  "))
        text = ' '.join(chunk for chunk in chunks if chunk)
        
        return text
    except ImportError:
        # If BeautifulSoup is not available, do simple HTML tag removal
        logger.warning("BeautifulSoup not available, falling back to simple HTML parsing")
        # Remove HTML tags using regex
        text = re.sub(r'<[^>]+>', ' ', html_content)
        # Clean up whitespace
        text = re.sub(r'\s+', ' ', text).strip()
        return text

def extract_text_from_file(file_path):
    """Extract text content from a file based on its extension"""
    file_ext = os.path.splitext(file_path)[1].lower()
    
    try:
        with open(file_path, 'r', encoding='utf-8', errors='replace') as file:
            content = file.read()
            
            if file_ext in ['.html', '.htm']:
                return extract_text_from_html(content)
            elif file_ext in ['.lisp', '.cl']:
                # For Lisp code, preserve the structure
                return content
            elif file_ext in ['.txt', '.md']:
                # For plain text, preserve as is
                return content
            else:
                # For unknown file types, just return the raw content
                return content
    except Exception as e:
        logger.error(f"Error processing {file_path}: {e}")
        return ""

def get_metadata(file_path, root_dir):
    """Extract metadata from file path"""
    rel_path = os.path.relpath(file_path, root_dir)
    file_name = os.path.basename(file_path)
    file_ext = os.path.splitext(file_name)[1].lower()
    
    # Determine content type based on directory structure and extension
    content_type = "unknown"
    
    if "yadd-reference" in rel_path:
        content_type = "reference"
    elif "gornschool-training" in rel_path:
        content_type = "tutorial"
    elif file_ext == '.txt' and file_name == 'usage.txt':
        content_type = "usage_guide"
        
    # Identify the topic more specifically
    topic = "general"
    
    if "yadd-reference/package-dokumentations" in rel_path:
        # Package documentation - identify which package
        package_match = re.search(r'package-dokumentations/(\d+)', rel_path)
        if package_match:
            package_id = package_match.group(1)
            topic = f"package_{package_id}"
            
            # Get more specific classification
            if "function-docs" in rel_path:
                topic += "_functions"
            elif "object-docs" in rel_path:
                topic += "_objects"
            elif "variable-docs" in rel_path:
                topic += "_variables"
                
    elif "gornschool-training/t" in rel_path:
        # Training materials - identify which tutorial
        tutorial_match = re.search(r't(\d+)', rel_path)
        if tutorial_match:
            topic = f"tutorial_{tutorial_match.group(1)}"
            
            # If it's a Lisp file, try to determine what it's about
            if file_ext == '.lisp':
                if "package.lisp" in file_name:
                    topic += "_package"
                elif re.search(r'(define-object|defclass)', extract_text_from_file(file_path), re.IGNORECASE):
                    topic += "_object_definition"
    
    return {
        "source": rel_path,
        "file_name": file_name,
        "file_type": file_ext.lstrip('.'),
        "content_type": content_type,
        "topic": topic
    }

def chunk_text(text, chunk_size, chunk_overlap):
    """Split text into manageable chunks with overlap"""
    if len(text) <= chunk_size:
        return [text]
    
    chunks = []
    start = 0
    
    while start < len(text):
        # Get a chunk of maximum size
        end = min(start + chunk_size, len(text))
        
        # If not at the end and we're in the middle of a word, find the last space
        if end < len(text):
            # Try to find a period, newline, or other natural break first
            natural_break = max(
                text.rfind('. ', start, end),
                text.rfind('\n', start, end),
                text.rfind(';; ', start, end),  # Common in Lisp comments
                text.rfind(')\n', start, end)   # End of a Lisp expression
            )
            
            if natural_break != -1 and natural_break > start + chunk_size // 2:
                # We found a good natural break
                end = natural_break + 1  # Include the period or newline
            else:
                # Fall back to finding the last space
                last_space = text.rfind(' ', start, end)
                if last_space != -1 and last_space > start + chunk_size // 2:
                    end = last_space
        
        # Add the chunk to our list
        chunks.append(text[start:end].strip())
        
        # Move the start position, accounting for overlap
        start = max(start + (chunk_size - chunk_overlap), end - chunk_overlap)
        # Ensure we're not stuck in an infinite loop if overlap is too large
        if start >= end:
            start = end
    
    return chunks

def create_document_id(doc):
    """Create a unique ID for a document chunk"""
    contents = f"{doc['metadata']['source']}_{doc['metadata']['chunk_id']}"
    return hashlib.md5(contents.encode()).hexdigest()

def process_directory(input_dir, output_dir, chunk_size, chunk_overlap):
    """Process all relevant files in the directory"""
    os.makedirs(output_dir, exist_ok=True)
    
    all_documents = []
    
    # Define file patterns to process
    file_patterns = ['*.lisp', '*.cl', '*.txt', '*.html', '*.htm', '*.md']
    
    # First, process the root usage.txt file if it exists
    usage_file = os.path.join(input_dir, 'usage.txt')
    if os.path.exists(usage_file):
        logger.info(f"Processing usage guide: {usage_file}")
        text = extract_text_from_file(usage_file)
        
        if text:
            metadata = get_metadata(usage_file, input_dir)
            text_chunks = chunk_text(text, chunk_size, chunk_overlap)
            
            for i, chunk in enumerate(text_chunks):
                doc = {
                    "text": chunk,
                    "metadata": metadata.copy()
                }
                doc["metadata"]["chunk_id"] = i
                doc["metadata"]["chunk_count"] = len(text_chunks)
                doc["id"] = create_document_id(doc)
                all_documents.append(doc)
    
    # Process all other documentation files
    for pattern in file_patterns:
        for file_path in glob.glob(os.path.join(input_dir, '**', pattern), recursive=True):
            # Skip certain directories that might contain unnecessary files
            if any(skip_dir in file_path for skip_dir in ['.git', 'node_modules', '__pycache__']):
                continue
                
            logger.info(f"Processing {file_path}")
            text = extract_text_from_file(file_path)
            
            if text:
                metadata = get_metadata(file_path, input_dir)
                
                # Use different chunking strategies based on file type
                if metadata['file_type'] == 'lisp' or metadata['file_type'] == 'cl':
                    # For Lisp files, try to chunk on define-object or other major forms
                    sections = re.split(r'(\(define-[^\s(]+|\(defclass|\(defmacro|\(defun)', text)
                    
                    # First segment might be comments or package definitions
                    if sections and sections[0].strip():
                        text_chunks = chunk_text(sections[0], chunk_size, chunk_overlap)
                        for i, chunk in enumerate(text_chunks):
                            doc = {
                                "text": chunk,
                                "metadata": metadata.copy()
                            }
                            doc["metadata"]["chunk_id"] = i
                            doc["metadata"]["section"] = "header"
                            doc["id"] = create_document_id(doc)
                            all_documents.append(doc)
                    
                    # Process the rest of the sections
                    for i in range(1, len(sections), 2):
                        if i+1 < len(sections):
                            section_text = sections[i] + sections[i+1]
                            
                            # Extract definition name if possible
                            name_match = re.search(r'\([^\s(]+\s+([^\s()]+)', section_text)
                            section_name = name_match.group(1) if name_match else f"section_{i//2}"
                            
                            doc = {
                                "text": section_text,
                                "metadata": metadata.copy()
                            }
                            doc["metadata"]["chunk_id"] = i // 2
                            doc["metadata"]["section"] = section_name
                            doc["id"] = create_document_id(doc)
                            all_documents.append(doc)
                else:
                    # For other files, use regular chunking
                    text_chunks = chunk_text(text, chunk_size, chunk_overlap)
                    
                    for i, chunk in enumerate(text_chunks):
                        doc = {
                            "text": chunk,
                            "metadata": metadata.copy()
                        }
                        doc["metadata"]["chunk_id"] = i
                        doc["metadata"]["chunk_count"] = len(text_chunks)
                        doc["id"] = create_document_id(doc)
                        all_documents.append(doc)
    
    # Save all documents
    documents_file = os.path.join(output_dir, "all_documents.json")
    with open(documents_file, 'w', encoding='utf-8') as f:
        json.dump(all_documents, f, indent=2)
    
    logger.info(f"Saved {len(all_documents)} documents to {documents_file}")
    
    # Create an inverted index for text search
    create_inverted_index(all_documents, output_dir)
    
    # Create topic-based files for browsing
    create_topic_files(all_documents, output_dir)
    
    # Create metadata file with general information
    create_metadata_file(all_documents, input_dir, output_dir)
    
    return all_documents

def create_inverted_index(documents, output_dir):
    """Create an inverted index for text search"""
    logger.info("Creating inverted index...")
    
    inverted_index = {}
    
    for doc in documents:
        # Tokenize the text into words
        words = re.findall(r'\b\w+\b', doc["text"].lower())
        
        # Remove duplicates and very common words
        stopwords = {'the', 'a', 'an', 'and', 'or', 'in', 'on', 'at', 'to', 'for', 'with', 'of', 'is', 'are', 'be', 'by', 'as', 'this', 'that'}
        unique_words = set(words) - stopwords
        
        # Add to inverted index
        for word in unique_words:
            if len(word) > 2:  # Skip very short words
                if word not in inverted_index:
                    inverted_index[word] = []
                inverted_index[word].append(doc["id"])
    
    # Save the inverted index
    index_file = os.path.join(output_dir, "inverted_index.json")
    with open(index_file, 'w', encoding='utf-8') as f:
        json.dump(inverted_index, f)
    
    logger.info(f"Created inverted index with {len(inverted_index)} terms")

def create_topic_files(documents, output_dir):
    """Create topic-based files for browsing"""
    logger.info("Creating topic-based files...")
    
    topics = {}
    for doc in documents:
        topic = doc["metadata"]["topic"]
        if topic not in topics:
            topics[topic] = []
        topics[topic].append(doc)
    
    topics_dir = os.path.join(output_dir, "topics")
    os.makedirs(topics_dir, exist_ok=True)
    
    # Create an index of topics
    topic_index = []
    
    for topic, docs in topics.items():
        topic_file = os.path.join(topics_dir, f"{topic}.md")
        with open(topic_file, 'w', encoding='utf-8') as f:
            f.write(f"# Gendl Documentation - {topic}\n\n")
            
            for doc in docs:
                f.write(f"## {doc['metadata']['file_name']}")
                
                if 'section' in doc['metadata']:
                    f.write(f" - {doc['metadata']['section']}")
                elif 'chunk_id' in doc['metadata'] and 'chunk_count' in doc['metadata']:
                    f.write(f" (chunk {doc['metadata']['chunk_id'] + 1}/{doc['metadata']['chunk_count']})")
                
                f.write(f"\n")
                f.write(f"Source: {doc['metadata']['source']}\n")
                f.write(f"Type: {doc['metadata']['content_type']}\n\n")
                f.write("```\n")
                f.write(doc["text"])
                f.write("\n```\n\n---\n\n")
        
        topic_index.append({
            "topic": topic,
            "document_count": len(docs),
            "file": f"{topic}.md"
        })
    
    # Save the topic index
    topic_index_file = os.path.join(output_dir, "topic_index.json")
    with open(topic_index_file, 'w', encoding='utf-8') as f:
        json.dump(topic_index, f, indent=2)
    
    logger.info(f"Created {len(topics)} topic files")

def create_metadata_file(documents, input_dir, output_dir):
    """Create metadata file with general information"""
    metadata_file = os.path.join(output_dir, "metadata.json")
    
    # Count documents by type
    content_types = {}
    for doc in documents:
        content_type = doc["metadata"]["content_type"]
        if content_type not in content_types:
            content_types[content_type] = 0
        content_types[content_type] += 1
    
    # Count documents by topic
    topics = {}
    for doc in documents:
        topic = doc["metadata"]["topic"]
        if topic not in topics:
            topics[topic] = 0
        topics[topic] += 1
    
    metadata = {
        "document_count": len(documents),
        "content_types": content_types,
        "topics": topics,
        "created_at": datetime.now().isoformat(),
        "source_directory": input_dir
    }
    
    with open(metadata_file, 'w', encoding='utf-8') as f:
        json.dump(metadata, f, indent=2)
    
    logger.info(f"Created metadata file with information about {len(documents)} documents")

def main():
    args = parse_arguments()
    
    logger.info(f"Starting Gendl documentation processing")
    logger.info(f"Input directory: {args.input}")
    logger.info(f"Output directory: {args.output}")
    
    try:
        # Install required packages if not available
        try:
            import bs4
        except ImportError:
            logger.info("Installing BeautifulSoup for HTML parsing...")
            import subprocess
            import sys
            subprocess.check_call([sys.executable, "-m", "pip", "install", "beautifulsoup4"])
            import bs4
            logger.info("Successfully installed BeautifulSoup")
        
        documents = process_directory(args.input, args.output, args.chunk_size, args.chunk_overlap)
        logger.info(f"Successfully processed {len(documents)} document chunks")
        logger.info(f"Gendl knowledge base created in {args.output}")
    except Exception as e:
        logger.error(f"Error processing documentation: {e}")
        import traceback
        logger.error(traceback.format_exc())
        return 1
    
    return 0

if __name__ == "__main__":
    exit(main())
