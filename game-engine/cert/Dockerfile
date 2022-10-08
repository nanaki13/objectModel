# A JDK 15 with Debian slim
FROM debian

RUN apt update \
	&& apt -y install openjdk-17-jdk \
	&& apt install pwgen

ENTRYPOINT [ "/bin/bash" ]