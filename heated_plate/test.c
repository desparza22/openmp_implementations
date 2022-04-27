#include <stdio.h>

int main(int argc, char* argv[]) {
  int arr[10][10];
  for(int i = 0; i < 5; i++) {
    printf("%p\n", &arr[i][0]);
  }
  for(int j = 0; j < 5; j++) {
    printf("%p\n", &arr[0][j]);
  }
  return 0;
}
