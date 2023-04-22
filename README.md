# `petit-cloud`

> Deploy Scheme web application easily

![](https://unsplash.com/photos/q4TfWtnz_xw/download?ixid=MnwxMjA3fDB8MXxzZWFyY2h8MzJ8fGNsb3VkJTIwdHJhbnNwYXJlbnQlMjBib3h8ZW58MHx8fHwxNjgxMzgzMTQw&force=true&w=830)

## Quickstart

Install FoundationDB:

```shell
wget https://github.com/apple/foundationdb/releases/download/7.1.30/foundationdb-clients_7.1.30-1_amd64.deb
wget https://github.com/apple/foundationdb/releases/download/7.1.30/foundationdb-server_7.1.30-1_amd64.deb
sudo dpkg -i foundationdb*
```

In a first terminal do:

```shell
  $ letloop exec petit-cloud-server.scm -- 127.0.0.1 9999
```

In another terminal:

```shell
  $ curl http://localhost:9999
  hello world
  $ letloop exec petit-cloud.scm -- http://localhost/_/api/v0/ example.scm
  Please, input the secret: foobar
  Good! What is done, is not to be done!
  $ curl http://localhost:9999
  hello world 1180 from example application
  $ curl http://localhost:9999
  hello world 1181 from example application
  $
```

## Kickstart

> ⚠️ 🚧 work in progress, use at your own risks 🚧 ⚠️

The get started you need to:

- Buy a domain;
- Buy a Virtual Private Server with Ubuntu, it is recommended to have
  two cores, and 8GB of RAM;

Make sure you have installed `ssh` on your own machine.

Next steps:

1. setup the server;
2. test it with a hello world application.

### Setup the server

Let's install `petit-cloud-server` on the `VPS`.

0. On the server download, install, and configure FoundationDB:

   ```shell
   ssh myserver.example
   wget https://github.com/apple/foundationdb/releases/download/7.1.30/foundationdb-clients_7.1.30-1_amd64.deb
   wget https://github.com/apple/foundationdb/releases/download/7.1.30/foundationdb-server_7.1.30-1_amd64.deb
   ```
   
   Install it:
   
   ```shell
   sudo dpkg -i foundationdb*.deb
   ```
   
   Configure `ssd` storage backend:
   
   ```shell
   fdbcli --exec 'configure ssd'
   ```

1. Download the [latest release of `petit-cloud` and
   `petit-cloud-server`](https://github.com/letloop/petit-cloud/releases/latest).

2. Upload `petit-cloud-server` in your virtual private server with something like:

   ```shell
   sftp myserver.example
   > put petite-cloud-server
   ```

3. Via `ssh`, copy the binary inside `/usr/local/bin/`:

   ```shell
   ssh myserver.example
   mkdir -p /usr/local/bin/
   sudo cp petite-cloud-server /usr/local/bin/
   ```

5. Install `nginx`, configure it using the directive `proxy_pass`
   to forward requests to local host, and port 9999;

4. To start the application server on startup, in the directory
   `/etc/systemd/system/` create a file called
   `petit-cloud-server.service`:
   
   ```conf
   [Unit]
   After=network.target
   StartLimitIntervalSec=0
   
   [Service]
   Type=simple
   Restart=always
   RestartSec=1
   User=www
   ExecStart=/usr/local/bin/petit-cloud-server

   [Install]
   WantedBy=multi-user.target
   ```

   Then execute the following commands:
   
   ```shell
   systemctl start petit-cloud-server
   systemctl enable petit-cloud-server
   ```

## [Support](mailto:amirouche@hyper.dev)

## LICENSE

Copyright © 2023 Amirouche BOUBEKKI

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice (including the
next paragraph) shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
