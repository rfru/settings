package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"
)

func main() {
	port := 8080
	if len(os.Args) == 2 {
		newPort, err := strconv.ParseInt(os.Args[1], 10, 64)
		if err == nil {
			port = int(newPort)
		}
	}
	fmt.Printf("Serving files in the current directory on port %d\n", port)
	wd, _ := os.Getwd()
	http.Handle("/", http.FileServer(http.Dir(wd)))
	err := http.ListenAndServe(fmt.Sprintf(":%d", port), nil)
	if err != nil {
		log.Fatal("ListenAndServe: ", err)
	}
}
