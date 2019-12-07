#!/bin/bash
set -e
stack build
stack exec rfid-kodi-control-exe
