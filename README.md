erl_eureka
=====

An OTP application

Build
-----

    $ rebar3 compile

Following are the REST operations available for non-java applications to use Eureka.

**appID** is the name of the application and **instanceID** is the unique id associated with the instance. In AWS cloud, instanceID is the **instance id** of the instance and in other data centers, it is the **hostname** of the instance.

For JSON/XML, the content types supplied must be **application/xml** or **application/json**.

| *Operation* | *HTTP action* | *Description* | *erlang function* |
| Register new application instance | POST /eureka/v2/apps/**appID** | Input: JSON/XML payload HTTP Code: 204 on success |erl_eureka_srv:register_host("UP")|
| De-register application instance | DELETE /eureka/v2/apps/**appID**/**instanceID** | HTTP Code: 200 on success |erl_eureka_srv:de_register()|
| Send application instance heartbeat | PUT /eureka/v2/apps/**appID**/**instanceID** | HTTP Code:
* 200 on success
* 404 if **instanceID** doesn't exist |erl_eureka_srv:heartbeat()|
| Query for all instances | GET /eureka/v2/apps | HTTP Code: 200 on success Output: JSON/XML|
| Query for all **appID** instances | GET /eureka/v2/apps/**appID** | HTTP Code: 200 on success Output: JSON/XML |erl_eureka_srv:app()|
| Query for a specific **appID**/**instanceID** | GET /eureka/v2/apps/**appID**/**instanceID** | HTTP Code: 200 on success Output: JSON/XML|erl_eureka_srv:app_instance()|
| Query for a specific **instanceID** | GET /eureka/v2/instances/**instanceID** | HTTP Code: 200 on success Output: JSON/XML|erl_eureka_srv:instance()|
| Take instance out of service | PUT /eureka/v2/apps/**appID**/**instanceID**/status?value=OUT_OF_SERVICE| HTTP Code:
* 200 on success
* 500 on failure |erl_eureka_srv:update_status_put()|
| Put instance back into service (remove override) | DELETE /eureka/v2/apps/**appID**/**instanceID**/status?value=UP  (The value=UP is optional, it is used as a suggestion for the fallback status due to removal of the override)| HTTP Code:
* 200 on success
* 500 on failure |erl_eureka_srv:update_status_delete()|
| Update metadata | PUT /eureka/v2/apps/**appID**/**instanceID**/metadata?key=value| HTTP Code:
* 200 on success
* 500 on failure |
| Query for all instances under a particular **vip address** | GET /eureka/v2/vips/**vipAddress** | 
* HTTP Code: 200 on success Output: JSON/XML 
* 404 if the **vipAddress** does not exist.|erl_eureka_srv:vip()|
| Query for all instances under a particular **secure vip address** | GET /eureka/v2/svips/**svipAddress** | 
* HTTP Code: 200 on success Output: JSON/XML 
* 404 if the **svipAddress** does not exist.|erl_eureka_srv:svip()|
