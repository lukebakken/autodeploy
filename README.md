autodeploy
==========

Erlang REST server to accept GitHub webhooks and autodeploy apps.

deployment
==========

```sh
sudo su - autodeploy
pushd autodeploy-src
git clean -fxd && git pull --rebase
source /home/erlang/installs/17.4/activate
make release
popd
mv autodeploy autodeploy-bak
mkdir autodeploy
cp -a autodeploy-src/_rel/autodeploy_release/* autodeploy/
cp autodeploy-bak/releases/1/sys.config autodeploy/releases/1/sys.config

# exit su shell
sudo monit restart autodeploy
```

