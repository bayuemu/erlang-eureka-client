erl_eureka
=====

An OTP application, a client of spring-boot-eureka for erlang.

Build
-----

    $ rebar3 compile

Following are the REST operations available for non-java applications to use Eureka.

**appID** is the name of the application and **instanceID** is the unique id associated with the instance. In AWS cloud, instanceID is the **instance id** of the instance and in other data centers, it is the **hostname** of the instance.

For JSON/XML, the content types supplied must be **application/xml** or **application/json**.
<table>
	<tr>
		<td>*Operation*</td>
		<td>*HTTP action*</td>
		<td>*Description* </td>
		<td>*erlang function*</td>
	</tr>
	<tr>
		<td>Register new application instance </td>
		<td> POST /eureka/v2/apps/**appID**</td>
		<td>Input: JSON/XML payload HTTP Code: 204 on success</td>
		<td>erl_eureka_srv:register_host("UP")</td>
	</tr>
	<tr>
		<td> De-register application instance</td>
		<td> DELETE /eureka/v2/apps/**appID**/**instanceID** </td>
		<td>HTTP Code: 200 on success</td>
		<td>erl_eureka_srv:de_register()</td>
	</tr>
	<tr>
		<td> Send application instance heartbeat</td>
		<td> PUT /eureka/v2/apps/**appID**/**instanceID** </td>
		<td>HTTP Code:
* 200 on success
* 404 if **instanceID** doesn't exist </td>
		<td>erl_eureka_srv:heartbeat()</td>
	</tr>
	<tr>
		<td>Query for all instances</td>
		<td>  GET /eureka/v2/apps</td>
		<td>HTTP Code: 200 on success Output: JSON/XML</td>
		<td> </td>
	</tr>
	<tr>
		<td>Query for all **appID** instances</td>
		<td> GET /eureka/v2/apps/**appID** </td>
		<td> HTTP Code: 200 on success Output: JSON/XML </td>
		<td> erl_eureka_srv:app()</td>
	</tr>
	<tr>
		<td>Query for a specific **appID**/**instanceID**</td>
		<td> GET /eureka/v2/apps/**appID**/**instanceID** </td>
		<td> HTTP Code: 200 on success Output: JSON/XML </td>
		<td>erl_eureka_srv:app_instance()</td>
	</tr>
	<tr>
		<td>Query for a specific **instanceID**</td>
		<td> GET /eureka/v2/instances/**instanceID** </td>
		<td>HTTP Code: 200 on success Output: JSON/XML </td>
		<td>erl_eureka_srv:instance()</td>
	</tr>
	<tr>
		<td>Take instance out of service</td>
		<td> PUT /eureka/v2/apps/**appID**/**instanceID**/status?value=OUT_OF_SERVICE</td>
		<td>HTTP Code:
* 200 on success
* 500 on failure </td>
		<td>erl_eureka_srv:update_status_put("OUT_OF_SERVICE")</td>
	</tr>
	<tr>
		<td>Put instance back into service (remove override) </td>
		<td> DELETE /eureka/v2/apps/**appID**/**instanceID**/status?value=UP  (The value=UP is optional, it is used as a suggestion for the fallback status due to removal of the override) </td>
		<td>HTTP Code:
* 200 on success
* 500 on failure </td>
		<td>erl_eureka_srv:update_status_delete("UP")</td>
	</tr>
	<tr>
		<td>Update metadata </td>
		<td> PUT /eureka/v2/apps/**appID**/**instanceID**/metadata?key=value </td>
		<td> HTTP Code:
* 200 on success
* 500 on failure  </td>
		<td></td>
	</tr>
	<tr>
		<td>Query for all instances under a particular **vip address** </td>
		<td>GET /eureka/v2/vips/**vipAddress** </td>
		<td>* HTTP Code: 200 on success Output: JSON/XML 
* 404 if the **vipAddress** does not exist. </td>
		<td>erl_eureka_srv:vip()</td>
	</tr>
	<tr>
		<td>Query for all instances under a particular **secure vip address**</td>
		<td>  GET /eureka/v2/svips/**svipAddress** </td>
		<td>* HTTP Code: 200 on success Output: JSON/XML 
* 404 if the **svipAddress** does not exist. </td>
		<td>erl_eureka_srv:svip()</td>
	</tr>
</table>
 
 
