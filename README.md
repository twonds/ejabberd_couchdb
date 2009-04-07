<h1> Ejabberd storage and authentication using couchdb </h1>

This is a collection or erlang modules for providing couchdb support for ejabberd. Similar to the odbc and ldap modules in the ejabberd distribution.

<h2>Authentication Schema </h2>

{
  _id: "tofu",
  _rev: "...",
  email: "tofu@collecta.com",
  password: "SHA1:sha1-hash"
}

<h1> INSTALL </h1>

<ul>
<li>Install ejabberd  
 NOTE: If you do not use source make sure you get developement packages.
</li>
<li>Install ecouch - located at the following url :
http://code.google.com/p/ecouch/
</li>
<li> Install ejabberd-couchdb 
 <br/>
 ./bootstrap.sh ;./configure;make install
</li>
<li>Configure ejabberd

</li>

</ul>




