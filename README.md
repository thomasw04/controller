# IoT Device Controller

A Haskell/Yesod HTTP service for controlling my IoT devices at home.
Currently supports Shelly Plug Gen3.

## Configuration

Register devices in `config.toml`. Each device has a **kind** and a **type**:

- **Kind** defines the API exposed for the device.
- **Type** selects the device-specific implementation of that API.
- **IP** selects the IP address to use for communication with this device.

Authenticate API requests with the token defined in `config.toml`.

## Docker / Podman

Set the secret's `file` in `compose.yaml` to your config file.

```sh
docker compose up -d --build
```

The server is available at `http://127.0.0.1:8080`.

## Security

This controller is intended for home networks. I do not recommend exposing it
to the internet: authentication uses a single shared token, and I am unlikely
to actively test or maintain this project. **Use at your own risk!**

## License
This project is licensed under the [MIT license](LICENSE).
