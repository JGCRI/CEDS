#!/bin/bash
#
# Searches all csv files in current directory and all children, checking if
# they end with a new line. The optional argument [-f] will add the new line
# if it is missing.
#
# Caleb Braun
# 6/18/19

declare -a needs_fix   # Array of files needing fixing
fix_flag='false'       # Boolean - do we fix these files?

# Print the arguments
print_usage() {
  printf "Usage: test_csv_eof.sh [-f]\n\nArguments:\n\t-f\tfix files\n"
}

# Parse arguments
while getopts 'f' flag; do
  case "${flag}" in
    f) fix_flag='true' ;;
    *) print_usage
       exit 1 ;;
  esac
done

# Loop through all files recursively
while IFS= read -r csv; do
    # Get the last line of each csv
    c=$(tail -c 1 "$csv")

    # If the last line is not empty, add new line
    if [ "$c" != "" ]; then
        needs_fix+=("$csv")

        if [ "$fix_flag" = "true" ]; then
            echo "" >> $csv
		    echo "Added newline to $csv"
        fi
    fi
done < <(find . -iname '*.csv')

if [ ${#needs_fix[@]} -eq 0 ]; then
    echo "All files ok."
    exit 0
fi

# If we did not fix them, print which files need fixing
if [ "$fix_flag" = "false" ]; then
    echo "The following files do not terminate with new lines:"
    printf '%s\n' "${needs_fix[@]}"
    exit 1
fi
