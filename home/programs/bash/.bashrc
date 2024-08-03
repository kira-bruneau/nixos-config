# Immediately append commands to history
trap 'history -a' DEBUG

# Read unread history at every prompt
export PROMPT_COMMAND="history -n"

# Fix forward history searching
stty -ixon
