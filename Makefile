FRONTEND_STACK_PATH=$(shell cd frontend && stack path --dist-dir)/build/frontend-output/frontend-output.jsexe/

build: frontend backend

frontend:
	mkdir -p backend/static
	cd frontend && stack build
	ln -nfs $(PWD)/frontend/$(FRONTEND_STACK_PATH) backend/static/jsexe

frontend-watch:
	cd frontend && stack build --file-watch --fast

ghcjsi:
	cd frontend && ([ -d node_modules ] || npm install socket.io)
	cd frontend && NODE_PATH=./node_modules stack ghci

backend:
	cd backend && stack build

.PHONY: build frontend backend
