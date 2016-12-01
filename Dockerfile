FROM java:8-alpine
MAINTAINER Gerard Klijs <gerard@openweb.nl>

ADD target/uberjar/snake.jar /snake/app.jar

EXPOSE 3000

CMD ["java", "-jar", "/snake/app.jar"]
