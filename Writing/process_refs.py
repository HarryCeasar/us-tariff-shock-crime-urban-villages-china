import re

# 1. Update references/reference.bib
with open('references/reference.bib', 'r', encoding='utf-8') as f:
    bib = f.read()

entries = bib.split('\n@')
new_bib = [entries[0]] if not entries[0].startswith('@') else []
start_idx = 0 if entries[0].startswith('@') else 1

for i in range(start_idx, len(entries)):
    entry = '@' + entries[i] if not entries[i].startswith('@') else entries[i]
    if re.search(r'[\u4e00-\u9fa5]', entry):
        if 'keywords = ' in entry or 'keywords=' in entry:
            pass # Replace or handle, assuming it's not
        else:
            # insert keyword before the last '}''
            entry = re.sub(r'(\s*)}$', r',\n  keywords = {zh}\n}', entry)
    else:
        if 'keywords' not in entry:
            entry = re.sub(r'(\s*)}$', r',\n  keywords = {en}\n}', entry)
    new_bib.append(entry)

with open('references/reference.bib', 'w', encoding='utf-8') as f:
    f.write('\n'.join(new_bib))
