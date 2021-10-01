build:
	docker stop $(docker ps -aq)
	docker system prune -f
	docker build -t python_package:latest .

run:
	docker run -it --device=/dev/ttyACM0 python_package:latest

