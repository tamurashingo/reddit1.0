FROM fukamachi/sbcl

RUN apt-get update && apt-get install -y libpq-dev

RUN mkdir /reddit
WORKDIR /reddit
COPY . /reddit

COPY entrypoint.sh /usr/bin/
RUN chmod +x /usr/bin/entrypoint.sh
ENTRYPOINT ["entrypoint.sh"]
EXPOSE 8000
EXPOSE 4005

CMD [ "ros", "run", \
      "-e", "(ql:quickload :swank)", \
      "-e", "(setf swank::*loopback-interface* \"0.0.0.0\")", \
      "-e", "(swank:create-server :dont-close t :style :spawn)"]
