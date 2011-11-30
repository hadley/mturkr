# mturkr package 

The `mturkr` package makes it easy to construct Amazon Mechanical Turk (MTurk) human interface tasks (HITs). It makes it easy to create templated questions, submit to amazon, monitor, review and aggregate answers.

It echoes the design of Amazon's [command line tools](http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkCLT/), but mapping components of the API to data structures and file formats that R developers and more familiar with.

Important terms:

* HIT: a single question
* HIT Type or Task: a group of questions of the same form
* Assignment: one question assigned to a person

## Key functions

* `init_task` - initialises a task directory with basic directory structure

* `publish_task` - combines `questions.xml` and `template.csv` to and
  publishes all HITs

* `view_task` - opens web browser pointing to one of your HITs

* `review_task` - downloads assignments needing review, and approves/rejects
  according to value of status column

* `unpublish_task` - removes all published HITs (in case you've made a mistake
  or were just testing)

Other useful functions include:

* `get_balance` to see how much money you have left.

Note that all these functions take a directory as their first argument (if you omit the directory it will use the last directory `mturkr` saw, so you can save some typing). 

By default all work is done in the sandbox. When you are ready to publish your questions to real workers, set `Host: production` in your `DESCRIPTION` file.

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

`mturkr` created:

* `hit-review.csv`: assignments needing review
* `hit-results.csv`: answers from approved assignments

## Useful amazon references

* Docs: http://aws.amazon.com/documentation/mturk/
* API: http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/
* Developer guide: http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMechanicalTurkRequester/
