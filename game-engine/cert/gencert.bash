#!/bin/bash

export PW=`pwgen -Bs 10 1`
echo $PW > password
ca="jonanakica"
domain="nanaki.hd.free.fr"
# Create a self signed key pair root CA certificate.
keytool -genkeypair -v \
  -alias $ca \
  -dname "CN=jonanakiCA, OU=Jonanaki Org, O=Jonanaki Company, L=Strasbourg, ST=BasRhin, C=FR" \
  -keystore $ca.jks \
  -keypass:env PW \
  -storepass:env PW \
  -keyalg RSA \
  -keysize 4096 \
  -ext KeyUsage:critical="keyCertSign" \
  -ext BasicConstraints:critical="ca:true" \
  -validity 9999

# Export the exampleCA public certificate as $ca.crt so that it can be used in trust stores.
keytool -export -v \
  -alias $ca \
  -file $ca.crt \
  -keypass:env PW \
  -storepass:env PW \
  -keystore $ca.jks \
  -rfc



  # Create a server certificate, tied to $domain
keytool -genkeypair -v \
  -alias $domain \
  -dname "CN=$domain, OU=Jonanaki Org, O=Jonanaki Company, L=Strasbourg, ST=BasRhin, C=FR" \
  -keystore $domain.jks \
  -keypass:env PW \
  -storepass:env PW \
  -keyalg RSA \
  -keysize 2048 \
  -validity 385

# Create a certificate signing request for $domain
keytool -certreq -v \
  -alias $domain \
  -keypass:env PW \
  -storepass:env PW \
  -keystore $domain.jks \
  -file $domain.csr

# Tell exampleCA to sign the $domain certificate. Note the extension is on the request, not the
# original certificate.
# Technically, keyUsage should be digitalSignature for DHE or ECDHE, keyEncipherment for RSA.
keytool -gencert -v \
  -alias $ca \
  -keypass:env PW \
  -storepass:env PW \
  -keystore $ca.jks \
  -infile $domain.csr \
  -outfile $domain.crt \
  -ext KeyUsage:critical="digitalSignature,keyEncipherment" \
  -ext EKU="serverAuth" \
  -ext SAN="DNS:$domain" \
  -rfc

# Tell $domain.jks it can trust $ca as a signer.
keytool -import -v \
  -alias $ca \
  -file $ca.crt \
  -keystore $domain.jks \
  -storetype JKS \
  -storepass:env PW << EOF
yes
EOF

# Import the signed certificate back into $domain.jks 
keytool -import -v \
  -alias $domain \
  -file $domain.crt \
  -keystore $domain.jks \
  -storetype JKS \
  -storepass:env PW

# List out the contents of $domain.jks just to confirm it.  
# If you are using Play as a TLS termination point, this is the key store you should present as the server.
keytool -list -v \
  -keystore $domain.jks \
  -storepass:env PW


# Export $domain's public certificate for use with nginx.
keytool -export -v \
  -alias $domain \
  -file $domain.crt \
  -keypass:env PW \
  -storepass:env PW \
  -keystore $domain.jks \
  -rfc

# Create a PKCS#12 keystore containing the public and private keys.
keytool -importkeystore -v \
  -srcalias $domain \
  -srckeystore $domain.jks \
  -srcstoretype jks \
  -srcstorepass:env PW \
  -destkeystore $domain.p12 \
  -destkeypass:env PW \
  -deststorepass:env PW \
  -deststoretype PKCS12