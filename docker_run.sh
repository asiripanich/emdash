docker stop emdash 
docker rm emdash
docker run -d -p 80:80 --name emdash emdash
