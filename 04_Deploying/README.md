### Building your own Shiny server with Docker containers

I wrote a dockerfile that installs an ubuntu based shiny server instance and runs the whole repo at localhost:3838

Here's how to build the image:
```bash
docker build -t shinies 04_Deploying/
```
and here's how to run the image in a container, linking your local repo to the right directory in the server build:

```bash
docker run -p 3838:3838 -v .:/srv/shiny-server/ shinies
```
