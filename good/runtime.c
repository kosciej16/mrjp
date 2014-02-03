#include <stdio.h>
#include <string.h>
#include <stdlib.h>   

#define BUFSIZE 1024
#define VARSIZE 4
char buf[BUFSIZE + 1];
extern struct _IO_FILE *stdin;
char* strcat(char*, const char*);
char* strcpy(char*, const char*);

void empty(){}

void printInt(int i){
  printf("%d\n",i);
}
void printString(char* s){
  printf("%s\n",s);
}

const char* readString(){
  scanf(" ");
  fgets(buf, BUFSIZE, stdin);
  int n = strlen(buf);
  if (n>0 && buf[n-1]==10) buf[n-1]=0;
  char* c = (char*)malloc(strlen(buf) + 1);
  strcpy(c,buf);
  return c;
}
int readInt(){
  int i;
  scanf("%d",&i);
  return i;
}

int eqString(char* s1, char* s2){
  if (!(strcmp(s1,s2))) return 1;
  else return 0;
}

const char* concat(const char* a, const char* b) {
  char* c = (char*)malloc(strlen(a) + strlen(b) + 1);
  strcpy(c,a);
  strcat(c,b);
  return c;
}

void* allocate_array(int n){
	int* res = malloc(n*VARSIZE);
	memset(res,0,n);
	return res;
}
