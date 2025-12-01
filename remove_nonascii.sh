#!/bin/bash
# Remove non-ASCII characters and null bytes from a file
# Usage: ./remove_nonascii.sh <filename>

if [ $# -eq 0 ]; then
    echo "Usage: $0 <filename>"
    exit 1
fi

FILE="$1"

if [ ! -f "$FILE" ]; then
    echo "Error: File '$FILE' not found"
    exit 1
fi

# Create backup
cp "$FILE" "$FILE.backup"

# Remove non-ASCII characters (keeps only ASCII 0-127) and null bytes
LC_ALL=C sed 's/[^\x00-\x7F]//g' "$FILE.backup" | tr -d '\000' > "$FILE"

echo "Non-ASCII characters and null bytes removed from $FILE"
echo "Backup saved as $FILE.backup"
