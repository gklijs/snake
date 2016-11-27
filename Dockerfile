FROM java:8-alpine
MAINTAINER Your Name <you@example.com>

ADD target/uberjar/snake.jar /snake/app.jar

EXPOSE 3000

CMD ["java", "-jar", "/snake/app.jar"]
