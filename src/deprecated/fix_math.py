import os
import shutil
import re

"""
This script was used to migrate an old layout to a newer one
It's no longer needed.
"""

ex = os.path.exists

def strip_leading_dig_space(s:str) -> str:
    return re.match(r'\d*\s*(.+)', s).groups()[0]

def multi_strip(s:str)->str:
    return '/'.join(map(strip_leading_dig_space, s.split('/')))

def rem_tags(s:str)->str:
    for tag in ["Prop", "Def", "Exercise", "Example"]:
        s = s.replace(f"% TAG {tag}", "")
    return s

read_from = 'bkup_math/'
write_to = 'tst'

def rep(match):
    uid, disp = match.groups()
    new_uid = strip_leading_dig_space(uid.split('/')[-1])
    return f'\\ref{{{new_uid}|{disp}|referenced}}'

def doSomethingWithFile(pth: str) -> None:
    assert pth[-4:] == '.tex', pth
    pth_ = pth[:-4]

    with open(pth, 'r') as f:
        txt = f.read()

    end = pth_.split('/')[-1]
    is_intro = strip_leading_dig_space(end) == strip_leading_dig_space(pth.split('/')[-2])
    if is_intro or 'Exercise' in end or 'TAG PROP' in txt:
        txt_ = re.sub(r'\\href{([^}]+)}{([^}]+)}', rep, txt)
        txt_ = txt_.replace("effects|reference|",'effects|') # spotfix
        with open(write_to+pth,'w') as f:
            f.write(rem_tags(txt_))
        return None

    if end[0].isdigit():
        end = ' '.join(end.split(' ')[1:])

    reg = r"%\s*TAG\s*(Def|Prop|Exercise|Example)"
    match = re.match(reg, txt)
    if match:
        tag = match.groups()[0]
    else:
        tag = ''

    os.mkdir(write_to+pth_)
    txt_ = re.sub(r'\\href{([^}]+)}{([^}]+)}', rep, txt)

    with open(write_to+pth_+'/1 Content.tex','w') as f:
        f.write(rem_tags(txt_))

    forbid = ['Proof', 'Solution']
    mdata = [end] + ([tag] if tag  else []) + ([] if end in forbid else [end])
    if mdata[-1] == 'Solution' and len(mdata) > 1:
        breakpoint()
    with open(write_to+pth_+'/0','w') as f:
        f.write('\n'.join(mdata))


def doSomethingWithDir(pth: str) -> None:
    os.mkdir(write_to+pth)
    end = pth.split('/')[-1]
    if end[0].isdigit():
        end = ' '.join(end.split(' ')[1:])
    tag = 'Exercise' if 'Exercise' in end else ''

    is_prop = False
    for ch in os.listdir(pth):
        chpth =  os.path.join(pth,ch)
        if os.path.isfile(chpth):
            with open(chpth, 'r') as f:
                is_prop = is_prop or 'TAG Prop' in f.read()

    tag = 'Prop' if is_prop else tag
    forbid = ['Proof', 'Solution']

    mdata = [end] + ([tag] if tag else []) + ([] if end in forbid else [end])

    with open(write_to+pth+'/0','w') as f:
        f.write('\n'.join(mdata))


def subs(s:str):
    reg = r'\\ref{([^\|]+)\|([^\|]+)\|([^\|]+)}'
    def rep(match):
        uid, disp, comment = match.groups()
        new_uid = strip_leading_dig_space(uid.split('/')[-1])
        return f'\\ref{{{new_uid}|{disp}|{comment}}}'
    res = re.sub(reg, rep, s)
    return res

def main() -> None:
    if ex('tstbkup_math'):
        shutil.rmtree('tstbkup_math')
    os.makedirs(write_to + read_from)
    for root, dirs, files in os.walk(read_from):
        for filename in files:
            doSomethingWithFile(os.path.join(root, filename))
        for dirname in dirs:
            doSomethingWithDir(os.path.join(root, dirname))


if __name__ == '__main__':
    main()