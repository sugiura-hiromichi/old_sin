#include <stdio.h>
// unsigned int型は0以上の整数を表すのでi==0の次はunsigned int型の最大値となる
// 従ってwhileの評価i>=0が永遠にtrueとなるのでプログラムは停止しない
// iが常に正であるにも関わらず負の値が表示されるのは
// printf関数の%d修飾子はint型なので、iがunsigned
// int型からint型にキャストされる時に負の値となる場合があるから

int main() {
	int i = 5;
	while (i >= 0) {
		printf("i=%d\n", i);
		i--;
	}
	return 0;
}
