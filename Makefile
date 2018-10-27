docker:
	docker build -t reason-server .

docker-esy:
	docker build -t reason-server-esy -f Dockerfile-esy .

