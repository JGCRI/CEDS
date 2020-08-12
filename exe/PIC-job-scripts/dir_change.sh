#!/bin/bash

old_dir='/qfs/people/<user_name>'
new_dir='xxxxxxxxxxxxxxx'

sed -i "s|$old_dir|$new_dir|g" *


