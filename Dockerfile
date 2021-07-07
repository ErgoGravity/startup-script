FROM openjdk:8-jre-slim as builder
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update  2>/dev/null
RUN apt-get install -y --no-install-recommends curl apt-transport-https apt-utils bc dirmngr gnupg  2>/dev/null
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list && \
    echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list && \
    curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add 2>/dev/null

    # seems that dash package upgrade is broken in Debian, so we hold it's version before update
RUN echo "dash hold" | dpkg --set-selections
RUN apt-get update  2>/dev/null
RUN apt-get upgrade -y  2>/dev/null
RUN apt-get install -y --no-install-recommends sbt wget sed  2>/dev/null
WORKDIR /StartupScript
RUN wget https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-19.3.1/graalvm-ce-java8-linux-amd64-19.3.1.tar.gz && \
    tar -xf graalvm-ce-java8-linux-amd64-19.3.1.tar.gz
ENV JAVA_HOME="/StartupScript/graalvm-ce-java8-19.3.1"
ENV PATH="${JAVA_HOME}/bin:$PATH"
ADD ["./appkit/", "/StartupScript/appkit"]
WORKDIR /StartupScript/appkit
RUN sbt publishLocal
WORKDIR /StartupScript
ADD [".", "/StartupScript"]
WORKDIR /StartupScript
#COPY --from=builder-front /usr/src/app/build/ ./public/
RUN sbt assembly
RUN mv `find . -name startupscript-*.jar` /startup_script.jar
CMD ["java", "-jar", "/startup_script.jar"]
#ENTRYPOINT java -jar /home/ergo/startup_script.jar --proxy gateway

FROM openjdk:8-jre-slim
RUN adduser --disabled-password --home /home/ergo/ --uid 9052 --gecos "ErgoPlatform" ergo && \
    install -m 0750 -o ergo -g ergo  -d /home/ergo/mixer
COPY --from=builder /startup_script.jar /home/ergo/startup_script.jar
#COPY ./mixer/conf/application.conf /home/ergo/mixer/application.conf
RUN chown ergo:ergo /home/ergo/startup_script.jar
USER ergo
#EXPOSE 9000
WORKDIR /home/ergo
#ENTRYPOINT java -jar -D"config.file"=mixer/application.conf /home/ergo/ergo-mixer.jar
ENTRYPOINT java -jar /home/ergo/startup_script.jar --proxy gateway