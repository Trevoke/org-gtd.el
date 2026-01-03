#!/usr/bin/env bash

cd ~/src/vendor/melpazoid/
RECIPE='(org-gtd :fetcher github :repo "trevoke/org-gtd.el")' LOCAL_REPO='~/src/projects/org-gtd.el' make > ~/src/projects/org-gtd.el/org-gtd.log
