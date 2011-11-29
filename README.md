# mturkr package 

The `mturkr` package makes it easy to construct Amazon Mechanical Turk (MTurk) human interface tasks (HITs). It makes it easy to create templated questions, submit to amazon, monitor, review and aggregate answers.

It echoes the design of Amazon's [command line tools](http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkCLT/), but mapping components of the API to data structures and file formats that R developers and more familiar with.

Important terms:

* HIT: a single question
* HIT Type or Task: a group of questions of the same form
* Assignment: one question assigned to a person

## Design principles

A task is represented as a set of files in a directory (see below for details).

All functions are robust to broken connections and errors, and are hence idempotent (if a function has no errors, running it again will do nothing). If successful, each request appends a row to a csv file, otherwise `stop()`s on failure.

All functions have quiet option. If `FALSE` (the default), are pretty wordy - describing each HTTP request.

All tasks work in the sandbox until you specifically indicate otherwise.

## File structure

Each task lives in its own directory with files:

User created:

* `DESCRIPTION`: task metadata, dcf format. Uses mustache template.
* `questions.xml`: xml file for question. Uses mustache template.
* `quals-*/`: qualifications.  See below for details.
* `template.csv`: template data. Each column becomes mustache key.
  * would be better to use something other than csv so you could have nested
    keys (list of named lists?)
  * build automatically from separator (`.`?) in column names? 
  * have another data frame describes column names?
* ? `static`: directory of static files uploaded to S3

`mturkr` created:

* `hit.csv`:  list of hits
* `hit-review.csv`: list of hits to review
* `hit-results.csv`: list of hit results
* `qualification-results.csv`

First argument to all functions is the path to the task directory. If omitted, uses the last task.

## Useful amazon references

* Docs: http://aws.amazon.com/documentation/mturk/
* API: http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/
* Developer guide: http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMechanicalTurkRequester/
