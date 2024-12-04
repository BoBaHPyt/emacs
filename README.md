# Emacs python ready environment

This repo holds my configuration for emacs, it uses mostly elpy, magit
and a few keybindings for python development.

The file is under test.

## Emacs installation

> brew install emacs

## Configuration installation

Simply clone this repo wherever you want and make a soft link in your
home directory to the .emacs file.

> git clone https://github.com/BoBaHPyt/emacs.git ~/emacs

> ln -s ~/emacs/.emacs ~/.emacs
> 
> ln -s ~/emacs/snippets ~/.emacs.d/mysnippets
>
> ln -s ~/emacs/custompackages ~/.emacs.d/custompackages

Then run emacs and enjoy.

## Start here (emacs tutorial)

> C-h t

### Notes

I recommend installing iTerm for mac because the Meta key (heavily used in emacs)
bring us a lot of headaches. So once installed, in the iTerm -> preferences ->
profiles -> keys you can set the "left command key" as Esc+ and this key will
act as Meta in emacs.

The right command key is left for 3rd level selector purposes (heavily used in
Spanish like keyboard's layouts), for instance to insert @ and # characters.
