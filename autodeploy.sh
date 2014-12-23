#!/usr/bin/env bash
backup_dir='../cfgbak'
sys_config='./_rel/autodeploy_release/releases/1/sys.config'
mkdir "$backup_dir"
set -o errexit
case "$1" in
  'pre')
    now="$(date '+%Y%m%d_%H%M%S')"
    if [[ -f $sys_config ]]
    then
      mv -vf "$sys_config" "$backup_dir/sys-config-${now}.bak"
    fi
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

