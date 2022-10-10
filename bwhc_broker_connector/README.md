## bwHC/DNPM Broker Connector

Implementation of Query Service Connector adapted to Broker API for DNPM.

### Design/Function

This connector is designed to work either with [Samply.Beam.Connect](https://github.com/samply/beam-connect) or any analogous HTTP-proxying set-up as entry point into the DNPM communication infrastructure, given the following requirements are fulfilled:

The configured HTTP-Proxy provides a "Peer discovery" endpoint

<code>GET /sites</code>

which returns a configuration of the following JSON structure:

```javascript
{
  "sites": [
    {
      "id": "SiteA",
      "name": "Site A",
      "virtualhost": "site.a.virtual"
    },
    {
      "id": "SiteB",
      "name": "Site B",
      "virtualhost": "site.b.virtual"
    },
    ...
  ]
}

```
Each entry here describes a point-to-point channel among DNPM nodes, realized via virtual hosting in the Proxy.

Specifically, requests from a given DNPM node at Site A to another at B Site are sent by the connector to the HTTP-Proxy as

```
HTTP-VERB <proxy-url>/{URI}
Host: site.b.virtual
...
```
and expected to be thus proxied to the DNPM node at Site B.


### Configuration

The installation package comes with bwhcConnectorConfig.xml

```xml
<?xml version="1.0" encoding="UTF-8"?>
<ConnectorConfig>

  <!-- Local Site ID as also defined in central config -->
  <Site id="TODO"/>

  <!-- Base URL to DNPM-Proxy -->
  <Broker baseURL="http://localhost"/>

</ConnectorConfig>
```
to configure the connector component. 

Alternatively, the two configuration parameters can be set by adding the  JVM system properties <code>bwhc.connector.config.siteId</code> and <code>bwhc.connector.config.baseUrl</code> to the application's start-up command.





