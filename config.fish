# colors
set normal (set_color normal)
set magenta (set_color magenta)
set yellow (set_color yellow)
set green (set_color green)
set red (set_color red)
set gray (set_color -o black)

# Environment
env TERM=screen command

# Some hard brain commands
#abbr -a -- - 'cd -'

# Fish git prompt
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showuntrackedfiles 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow
set __fish_git_prompt_color_upstream_ahead green
set __fish_git_prompt_color_upstream_behind red

# Status Chars
set __fish_git_prompt_char_dirtystate '⚡'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_untrackedfiles '☡'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '+'
set __fish_git_prompt_char_upstream_behind '-'


function fish_prompt
        set last_status $status

        set_color $fish_color_cwd
        printf '%s' (prompt_pwd)
        set_color normal

        printf '%s ' (__fish_git_prompt)

        set_color normal
end

set -g fish_user_paths "/usr/local/opt/sqlite/bin" $fish_user_paths
set -g fish_user_paths "/usr/local/sbin" $fish_user_paths
set -g fish_user_paths "/opt/local/bin" $fish_user_paths
set -g fish_user_paths "~/Library/Python/2.7/bin" $fish_user_paths
set -g fish_user_paths "~/Bin" $fish_user_paths

# Algunas variables
set -gx EDITOR vim
set -gx WORKON_HOME "~/virtualenvs"
set -gx VIRTUALENVWRAPPER_HOOK_DIR $WORKON_HOME/hooks

