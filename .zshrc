### Added by Zinit's installer
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit
### End of Zinit's installer chunk

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/z-a-rust \
    zdharma-continuum/z-a-as-monitor \
    zdharma-continuum/z-a-patch-dl \
    zdharma-continuum/z-a-bin-gem-node

zinit wait lucid light-mode for \
  atinit"zicompinit; zicdreplay" \
      zdharma-continuum/fast-syntax-highlighting \
  atload"_zsh_autosuggest_start" \
      zsh-users/zsh-autosuggestions \
  blockf atpull'zinit creinstall -q .' \
      zsh-users/zsh-completions

zinit ice depth=1; zinit light romkatv/powerlevel10k

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

zinit light yonchu/grunt-zsh-completion
zinit light agkozak/zsh-z

zinit snippet OMZP::git

zinit lucid has'docker' for \
  as'completion' is-snippet \
  'https://github.com/docker/cli/blob/master/contrib/completion/zsh/_docker' \
  \
  as'completion' is-snippet \
  'https://github.com/docker/compose/blob/master/contrib/completion/zsh/_docker-compose' \

HISTFILE="$HOME/.zsh_history"
HISTSIZE=10000000
SAVEHIST=10000000
setopt BANG_HIST                 # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY          # Write the history file in the ":start:elapsed;command" format.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
#setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a line previously found.
setopt HIST_IGNORE_SPACE         # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS         # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS        # Remove superfluous blanks before recording entry.
setopt HIST_VERIFY               # Don't execute immediately upon history expansion.
setopt HIST_BEEP  

# Partial completion
zstyle ':completion:*' matcher-list 'r:|=*' 'l:|=* r:|=*'

alias top='/usr/bin/top -o %CPU'
alias xclip='xclip -selection c'
alias strongdebug='sudo journalctl -u strongswan -f'
alias mpd='systemctl restart mpd --user'
alias retourmicro='pactl load-module module-loopback latency_msec=1'
alias knownhosts="ssh-keygen -l -f ~/.ssh/known_hosts"
alias ll='exa -l'
alias ls='exa'
alias grep='rg'
alias piaup='sudo systemctl restart piavpn'
alias piadown='sudo systemctl stop piavpn'

runcontainer() {
    docker run --rm -it --entrypoint bash $*
}

cd() {
    if [ "$1" = "..." ]; then
	builtin cd ../..
    elif [ "$1" = "...." ]; then
	builtin cd ../../..
    else
	builtin cd $*
    fi
}

flac2mp3() {
    for FILE in *.flac;
    do
	ffmpeg -i "$FILE" -ab 320k -map_metadata 0 "${FILE%.*}.mp3";
    done
}

emacs() {
	# /usr/bin/emacs -fn inconsolata-11 $@ &
	nohup /usr/bin/emacs $@ > /tmp/nohup & disown
}

vlc() {
	nohup /usr/bin/vlc $@ > /tmp/nohup & disown
}

cert() {
	openssl x509 -in $@ -noout -text | more
}

magit() {
	nocorrect emacs --eval "(progn (magit-status) (delete-other-windows))"
}

ediff() {
    emacs --eval "(ediff-files \"$1\" \"$2\")"
}

export EDITOR=/usr/bin/joe

export PATH=${PATH}:/opt/java/bin:/opt/ruby1.9/bin:/home/gdenry/.cargo/bin:/home/gdenry/bin

export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'
export LESS_TERMCAP_ue=$'\E[0m'

setopt EXTENDED_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_FIND_NO_DUPS
setopt HIST_SAVE_NO_DUPS
setopt HIST_BEEP

export FZF_DEFAULT_COMMAND='fd --type f'

export TERMINAL="xterm"

export LESS="-FRXI"

export RIPGREP_CONFIG_PATH=~/.ripgreprc

fpath=(~/.zsh/functions $fpath)


export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"


source ~/.zshrc.work 2>/dev/null

# pnpm
export PNPM_HOME="/home/gdenry/.local/share/pnpm"
export PATH="$PNPM_HOME:$PATH"
# pnpm end