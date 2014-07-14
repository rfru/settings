package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
)

func main() {
	fmt.Println("Serving files in the current directory on port 8080")
	wd, _ := os.Getwd()
	http.Handle("/", http.FileServer(http.Dir(wd)))
	err := http.ListenAndServe(":8080", nil)
	if err != nil {
		log.Fatal("ListenAndServe: ", err)
	}
}
