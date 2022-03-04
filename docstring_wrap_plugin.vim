
let s:scriptdir = expand("<sfile>:p:h")

function! WrapDocstring()
    let l:where = getpos(".")
    let l:curofft = line2byte(".") + col(".") - 1
    ?"""?
    let l:startofft = line2byte(".") + col(".") - 1
    let l:indent = indent(".")
    call setpos(".", l:where)
    exe "?\"\"\"?1,/\"\"\"/-1!python '" . s:scriptdir . "/docstring_wrap.py' --linewise --indent=" . l:indent . " --offset=" . (l:curofft - l:startofft)
    let l:newpos = expand("<cword>")
    delete _
    exe "goto " . (l:startofft + l:newpos - 1)
    nohl
endfunction

nnoremap gqaa :call WrapDocstring()<CR>

" nnoremap gqaa ?"""?1<CR>O<ESC>V/"""/-1<CR>:!python ~/Settings/Emacs/firstparty/python-docstring-mode/docstring_wrap.py<CR>dd:nohl<CR>
