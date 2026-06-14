function fish_prompt --description 'Write out the prompt'
        set -l last_status $status
        set -l last_duration $CMD_DURATION
        set -l normal (set_color normal)
        set -l status_color (set_color brgreen)
        set -l cwd_color (set_color $fish_color_cwd)
        set -l vcs_color (set_color brpurple)
        set -l prompt_status ""
    
        # Since we display the prompt on a new line allow the directory names to be longer.
        set -q fish_prompt_pwd_dir_length
        or set -lx fish_prompt_pwd_dir_length 0
    
        # Color the prompt differently when we're root
        set -l suffix '❯'
        if functions -q fish_is_root_user; and fish_is_root_user
                if set -q fish_color_cwd_root
                        set cwd_color (set_color $fish_color_cwd_root)
                end
                set suffix '#'
        end
    
        # Color the prompt in red on error
        if test $last_status -ne 0
                set status_color (set_color $fish_color_error)
                set prompt_status $status_color "[" $last_status "]" $normal
        end

        # Format duration if it exists and is non-zero
        set -l duration_str ""
        if test -n "$last_duration" -a "$last_duration" -gt 0
            set -l seconds (math "$last_duration / 1000")
            set duration_str (set_color yellow) "took " (printf '%.2fs' $seconds) " " $normal
        end

        echo -s (prompt_login) ' ' $cwd_color (prompt_pwd) $vcs_color (fish_vcs_prompt) $normal ' ' $prompt_status ' ' $duration_str
        echo -n -s $status_color $suffix ' ' $normal
end
