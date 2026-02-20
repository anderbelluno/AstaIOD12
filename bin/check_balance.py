import sys
import re

def check_pascal_balance(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    # Remove all comments for counting
    content = re.sub(r'//.*', '', content)
    content = re.sub(r'{[^\$].*?}', '', content, flags=re.DOTALL)
    content = re.sub(r'\(\*.*?\*\)', '', content, flags=re.DOTALL)
    
    # Remove strings
    content = re.sub(r"'.*?'", "''", content)

    begins = re.findall(r'\bbegin\b', content, re.IGNORECASE)
    ends = re.findall(r'\bend\b', content, re.IGNORECASE)
    asms = re.findall(r'\basm\b', content, re.IGNORECASE)
    cases = re.findall(r'\bcase\b', content, re.IGNORECASE)
    records = re.findall(r'\brecord\b', content, re.IGNORECASE)
    # class can be 'class' or 'class(base)' or 'class of something'
    # we only want to count TMyClass = class ... end; 
    # and NOT 'TClass = class of TObject;'
    classes = re.findall(r'=\s*class\b', content, re.IGNORECASE)
    
    print(f"Begins: {len(begins)}")
    print(f"Ends: {len(ends)}")
    print(f"ASMs: {len(asms)}")
    print(f"Cases: {len(cases)}")
    print(f"Records: {len(records)}")
    print(f"Classes: {len(classes)}")
    
    total_starters = len(begins) + len(asms) + len(cases) + len(records) + len(classes)
    print(f"Total Starters: {total_starters}")
    print(f"Difference (Starters - Ends): {total_starters - len(ends)}")

if __name__ == "__main__":
    check_pascal_balance(sys.argv[1])
