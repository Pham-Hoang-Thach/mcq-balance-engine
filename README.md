# MCQ Balance Engine

The **MCQ Balance Engine** is an Emacs-based tool designed to shuffle multiple-choice question (MCQ) headings in Org-mode files, ensuring unique combinations of questions based on specified tags. It also extracts correct answers from Org-mode files and saves them as a CSV file for easy access and analysis.

# Features

- **Randomized Shuffling**: Uses the Fisher-Yates algorithm to shuffle question headings randomly.
- **Persistent Uniqueness**: Ensures that combinations of questions, selected by tags, remain unique across sessions.
- **Tag-Based Organization**: Supports organizing questions by tags, making it easy to select subsets of questions based on user input.
- **Customizable Output**: Allows you to specify whether tags should be displayed in the output.
- **Correct Answer Extraction**: Automatically extracts `:Correct:` tagged answers from Org-mode files and saves them as a CSV.

# Installation

To use the MCQ Balance Engine, simply download the following files:

1. `persistent-org-combo-shuffler.el`
2. `extract-correct-answers.el`

Place them in your Emacs `load-path` directory and load them into your Emacs environment.

```emacs-lisp
(load-file "/path/to/persistent-org-combo-shuffler.el")
(load-file "/path/to/extract-correct-answers.el")
```

Alternatively, you can copy the contents of these files into your Emacs configuration file.

# Usage

## Persistent Org Combo Shuffler

To start shuffling and selecting unique combinations of Org-mode headings by tags:

1. Open your Org-mode file in Emacs.
2. Run the command: `M-x persistent-org-combo-shuffler`
3. The engine will display a summary of available tags and their counts.
4. Enter your tag specifications (e.g., `math=2,science=3`).
5. The engine will generate a unique combination of headings, ensuring that no combination repeats.
6. You can choose whether to include tags in the output.

## Extract Correct Answers

To extract the correct answers and save them as a CSV:

1. Open your Org-mode file containing questions and answers.
2. Run the command: `M-x extract-correct-answers`
3. You will be prompted to enter a file path where the answers will be saved.
4. The answers will be extracted and saved in a CSV format with the structure: `Question, Answer`.

## Example

After running the `persistent-org-combo-shuffler` function, you may get a result like the following in a new buffer:

```org
* Exam code: 123

** Math: What is 2 + 2?
- Answer: 4

** Science: What is the boiling point of water?
- Answer: 100°C
```

# Customization

The **MCQ Balance Engine** stores previously used combinations and IDs to ensure that no combination is repeated.

You can configure the maximum number of attempts to generate a unique combination by modifying the `max-attempts` variable in the code.

# File Structure

- `persistent-org-combo-shuffler.el`: Contains functions for shuffling and displaying MCQ combinations.
- `extract-correct-answers.el`: Extracts correct answers marked by `:Correct:` and saves them to a CSV.
- `-unique-combinations.el`: A file storing previously generated combinations for the associated Org file.
- `.used-ids.el`: A file storing the used 3-digit IDs for the associated Org file.

# Demo

<iframe width="560" height="315" src="https://youtu.be/dl1TpdEbq7Q" frameborder="0" allowfullscreen></iframe>

# Disclaimer

This program is provided "as is", without any warranty of any kind, express or implied, including but not limited to the warranties of merchantability, fitness for a particular purpose, or non-infringement. In no event shall the authors or copyright holders be liable for any claim, damages, or other liability, whether in an action of contract, tort, or otherwise, arising from, out of, or in connection with the program or the use or other dealings in the code.

Use this code at your own risk. It is your responsibility to ensure that the generated combinations and associated data are accurate and appropriate for your use case.

