#!/usr/bin/env bash
backup_dir=../cfgbak
mkdir "$backup_dir"
set -o errexit
case "$1" in
  'pre')
    now="$(date '+%Y%m%d_%H%M%S')"
    mv -vf ./_rel/autodeploy_release/releases/1/sys.config date "$backup_dir/sys-config-${now}.bak"
    ;;
  'post')
    source /home/erlang/installs/17.4/activate
    make clean release
    ;;
  *)
    echo '[error] argument must be "pre" or "post"'
    exit 1
    ;;
esac

