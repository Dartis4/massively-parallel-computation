# massively-parallel-computation
Group code projects for the Massively Parallel Computation class. This repo contains our code for each of the basic OTP behaviors in Erlang and our final project. Many of the files in this repository use templates that were provided to us by our professor Lee Barney, however our team filled in these templates with our own code.

## Team Members:

- Dane Artis
- Gabriel Ikpaetuk
- Tyson Mergel
- Jared Perlic
- Kevin Ramos
- Sully Udall

## Final Project - Package Tracker

### Description

Our class project was to create an application that would be able to handle tracking a large number of packages being shipped as it travels from start to finish. The project is meant to mock what might exist in a large-scale courier service like FedEx or UPS. Our application is able to track packages, courier facilities, and courier vehicles. The end goal is to be able to handle thousands of packages and requests.

### Environment

All of the code in this project is written in Erlang and makes use of the Rebar3 build tool. The two applications in this repository that are components of this project are `package_tracker` and `stress_tester`. The system was developed to be run on Digital Ocean droplets running Ubuntu.

We used 3 droplets to create our complete system, however to make a more effective system and make use of the parallel nature of the project, additional droplets (or servers) would need to be created in order to overcome OS limitations on open file descriptors and open sockets.

The droplets fulfill the following main roles:

- The client
- The server
- The database

The client generates HTTP requests with messages that contain the data in JSON. The requests must call directly to each target extension of the server domain. The `stress_tester` serves as the client in this project and generates several thousand requests to simulate a given amount of packages being processed by the system.

The server uses the `cowboy` package for the web server, which handles all web requests. This then interfaces with the Riak Erlang client to make calls to the Riak key-value store within OTP behavior functions. Each request type has a corresponding Cowboy handler, `gen_statem` round-robin dispatcher, and `gen_server`, each of which can be duplicated within the supervision tree for load-balancing.

The database is a Riak key-value store. To handle load-balancing, several Riak nodes would need to be registered in a ring.
