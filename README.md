<h1> Ejabberd storage and authentication using couchdb </h1>

This is a collection of erlang modules for providing couchdb support for ejabberd. Similar to the odbc and ldap modules in the ejabberd distribution. Right now it only supports authentication.

<h2>Authentication Schema </h2>

{
  _id: "tofu@xmppserver.tld",
  _rev: "...",
  email: "tofu@collecta.com",
  password: "SHA1:sha1-hash"
}

<h1> INSTALL </h1>

<ol>
<li>Install ejabberd  
  <ul>
     <li>NOTE: If you do not use source make sure you get developement packages.
     </li>
  </ul>
</li>
<li>Install ecouch - 
   <ul>
    <li>located at the following url :
http://code.google.com/p/ecouch/
    </li>
   </ul>
</li>
<li> Install ejabberd-couchdb 
 <ul><li><pre>
 ./bootstrap.sh ;./configure;make install
</pre></li></ul>
</li>
<li>Configure ejabberd
   <ul>
     <li>
     Add couch as the authentication module.
     <pre>
{auth_method, couchdb}.
     </pre> 
     </li>
<li> Configure couchdb options.
<pre>
{couchdb_options, [
		  {host,"localhost"},
		  {port,"5489"},
		  {user, none},
		  {pass, none}	
		  ]}.
</pre>
   </ul>
</li>

</ol>




