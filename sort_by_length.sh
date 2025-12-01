#!/bin/bash
# Sort file by increasing line length
# Usage: ./sort_by_length.sh <filename> [output_file]

if [ $# -eq 0 ]; then
    echo "Usage: $0 <filename> [output_file]"
    exit 1
fi

INPUT_FILE="$1"
OUTPUT_FILE="${2:-${INPUT_FILE}.sorted}"

if [ ! -f "$INPUT_FILE" ]; then
    echo "Error: File '$INPUT_FILE' not found"
    exit 1
fi

# Sort by line length (shortest to longest)
awk '{ print length($0), $0 }' "$INPUT_FILE" | sort -n | cut -d' ' -f2- > "$OUTPUT_FILE"

echo "File sorted by line length"
echo "Output saved to: $OUTPUT_FILE"
