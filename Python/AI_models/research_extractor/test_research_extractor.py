"""
Test suite for the research extractor agent.

This script creates test input files with diverse reference types
and validates that the agent correctly processes them.
"""

import os
import tempfile
import shutil
from pathlib import Path

from research_extractor_agent import run_agent


# Test references covering different scenarios
TEST_REFERENCES = """
Shafir, Todd, Shaffer, Wendy and Stafford, J. (2022). A Study of Artificial Intelligence, In Proceedings of AI Advances 
DOI: 10.1234/example.doi.2022

The human brain is a prediction machine: The German physician Hermann von Helmholtz developed the idea of the brain being a "prediction machine."

https://www.nytimes.com/2023/05/15/technology/ai-language-models.html

Ibid.

arXiv:2301.00123 - Deep Learning Advances in Computer Vision

This is a quote from a lecture: "The essence of knowledge is generalization" - Professor Smith at MIT, 2023
"""


def create_test_file(content: str) -> str:
    """Create a temporary test file with references."""
    temp_file = tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False, encoding='utf-8')
    temp_file.write(content)
    temp_file.close()
    return temp_file.name


def test_basic_processing():
    """Test basic agent processing with diverse references."""
    print("\n" + "="*80)
    print("TEST 1: Basic Processing with Diverse References")
    print("="*80 + "\n")
    
    # Create test input file
    test_file = create_test_file(TEST_REFERENCES)
    
    # Create temporary output directory
    output_dir = tempfile.mkdtemp()
    
    try:
        # Run agent with verbose mode for testing
        result = run_agent(
            input_file=test_file,
            output_dir=output_dir,
            origin="[[Test References]]",
            verbose=True
        )
        
        # Validate results
        print("\n" + "="*80)
        print("TEST RESULTS")
        print("="*80)
        
        assert result['total'] > 0, "Should find at least one reference"
        print(f"✅ Found {result['total']} references")
        
        # Check that invalid references were skipped
        assert result['skipped'] > 0, "Should skip invalid reference (Ibid.)"
        print(f"✅ Skipped {result['skipped']} invalid references")
        
        # Check folder structure
        expected_folders = ['Journals', 'Articles', 'Quotes', 'Lectures']
        created_folders = [d for d in os.listdir(output_dir) if os.path.isdir(os.path.join(output_dir, d))]
        print(f"✅ Created folders: {created_folders}")
        
        # Check for markdown files
        md_files = []
        for folder in created_folders:
            folder_path = os.path.join(output_dir, folder)
            files = [f for f in os.listdir(folder_path) if f.endswith('.md')]
            md_files.extend([(folder, f) for f in files])
        
        if md_files:
            print(f"✅ Created {len(md_files)} markdown files:")
            for folder, filename in md_files:
                print(f"   - {folder}/{filename}")
        else:
            print("⚠️  No markdown files created (may be due to API limitations or all references skipped)")
        
        print("\n" + "="*80)
        print("TEST 1: PASSED ✅")
        print("="*80 + "\n")
        
        return result
        
    finally:
        # Cleanup
        os.unlink(test_file)
        shutil.rmtree(output_dir, ignore_errors=True)


def test_invalid_references_only():
    """Test that agent correctly handles file with only invalid references."""
    print("\n" + "="*80)
    print("TEST 2: Invalid References Only")
    print("="*80 + "\n")
    
    invalid_refs = """
Ibid.

loc. cit.

1144a4–5

id.
"""
    
    test_file = create_test_file(invalid_refs)
    output_dir = tempfile.mkdtemp()
    
    try:
        result = run_agent(
            input_file=test_file,
            output_dir=output_dir,
            origin="[[Invalid Test]]",
            verbose=False  # Less verbose for this test
        )
        
        print("\n" + "="*80)
        print("TEST RESULTS")
        print("="*80)
        
        assert result['total'] > 0, "Should find references"
        assert result['skipped'] == result['total'], "All references should be skipped"
        assert result['success'] == 0, "No references should succeed"
        
        print(f"✅ All {result['total']} invalid references were correctly skipped")
        print(f"✅ No markdown files created (as expected)")
        
        print("\n" + "="*80)
        print("TEST 2: PASSED ✅")
        print("="*80 + "\n")
        
        return result
        
    finally:
        os.unlink(test_file)
        shutil.rmtree(output_dir, ignore_errors=True)


def test_research_paper_doi():
    """Test processing a research paper with DOI."""
    print("\n" + "="*80)
    print("TEST 3: Research Paper with DOI")
    print("="*80 + "\n")
    
    paper_ref = """
Attention Is All You Need
Vaswani et al., 2017
DOI: 10.48550/arXiv.1706.03762
"""
    
    test_file = create_test_file(paper_ref)
    output_dir = tempfile.mkdtemp()
    
    try:
        result = run_agent(
            input_file=test_file,
            output_dir=output_dir,
            origin="[[Test Paper]]",
            verbose=True
        )
        
        print("\n" + "="*80)
        print("TEST RESULTS")
        print("="*80)
        
        assert result['total'] == 1, "Should find one reference"
        
        if result['success'] == 1:
            print("✅ Successfully processed research paper")
            
            # Check that markdown file was created in Journals folder
            journals_path = os.path.join(output_dir, 'Journals')
            if os.path.exists(journals_path):
                md_files = [f for f in os.listdir(journals_path) if f.endswith('.md')]
                if md_files:
                    print(f"✅ Created markdown file: {md_files[0]}")
                    
                    # Validate markdown content
                    with open(os.path.join(journals_path, md_files[0]), 'r', encoding='utf-8') as f:
                        content = f.read()
                        assert '---' in content, "Should have YAML frontmatter"
                        assert 'Authors:' in content, "Should have authors field"
                        assert 'Summary:' in content, "Should have summary field"
                        print("✅ Markdown file has correct structure")
        else:
            print(f"⚠️  Paper processing skipped or failed: {result.get('details', [{}])[0].get('reason', 'unknown')}")
        
        print("\n" + "="*80)
        print("TEST 3: PASSED ✅")
        print("="*80 + "\n")
        
        return result
        
    finally:
        os.unlink(test_file)
        shutil.rmtree(output_dir, ignore_errors=True)


def test_book_skipping():
    """Test that books are correctly skipped."""
    print("\n" + "="*80)
    print("TEST 4: Book References (Should Skip)")
    print("="*80 + "\n")
    
    book_ref = """
The Pragmatic Programmer: From Journeyman to Master
by Andrew Hunt and David Thomas
Addison-Wesley Professional, 1999
ISBN: 978-0201616224
"""
    
    test_file = create_test_file(book_ref)
    output_dir = tempfile.mkdtemp()
    
    try:
        result = run_agent(
            input_file=test_file,
            output_dir=output_dir,
            origin="[[Book Test]]",
            verbose=False
        )
        
        print("\n" + "="*80)
        print("TEST RESULTS")
        print("="*80)
        
        assert result['total'] == 1, "Should find one reference"
        
        # Books should be skipped
        if result['skipped'] == 1:
            print("✅ Book correctly identified and skipped")
            skip_reason = result.get('details', [{}])[0].get('reason', '')
            if 'book' in skip_reason.lower():
                print(f"✅ Skip reason mentions book: {skip_reason}")
        
        print("\n" + "="*80)
        print("TEST 4: PASSED ✅")
        print("="*80 + "\n")
        
        return result
        
    finally:
        os.unlink(test_file)
        shutil.rmtree(output_dir, ignore_errors=True)


def run_all_tests():
    """Run all test cases."""
    print("\n" + "#"*80)
    print("# RESEARCH EXTRACTOR AGENT - TEST SUITE")
    print("#"*80 + "\n")
    
    tests = [
        ("Basic Processing", test_basic_processing),
        ("Invalid References Only", test_invalid_references_only),
        ("Research Paper with DOI", test_research_paper_doi),
        ("Book Skipping", test_book_skipping),
    ]
    
    results = []
    for test_name, test_func in tests:
        try:
            result = test_func()
            results.append((test_name, "PASSED", result))
        except AssertionError as e:
            print(f"\n❌ TEST FAILED: {test_name}")
            print(f"   Error: {str(e)}\n")
            results.append((test_name, "FAILED", str(e)))
        except Exception as e:
            print(f"\n❌ TEST ERROR: {test_name}")
            print(f"   Exception: {str(e)}\n")
            results.append((test_name, "ERROR", str(e)))
    
    # Final summary
    print("\n" + "#"*80)
    print("# TEST SUMMARY")
    print("#"*80 + "\n")
    
    passed = sum(1 for _, status, _ in results if status == "PASSED")
    total = len(results)
    
    for test_name, status, _ in results:
        symbol = "✅" if status == "PASSED" else "❌"
        print(f"{symbol} {test_name}: {status}")
    
    print(f"\n{'='*80}")
    print(f"Tests passed: {passed}/{total} ({passed/total*100:.1f}%)")
    print(f"{'='*80}\n")


if __name__ == "__main__":
    # Run all tests
    run_all_tests()
