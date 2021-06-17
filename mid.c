
/*************
나비 그리기 
*************/

/*

#include <stdio.h>

int main(void)
{

int n;

scanf("%d", &n);

if(n % 2 == 1)
{

for(int i = 0; i < n/2; i++)
{
	for(int j = 0; j <= i; j++)
	{
		printf("*");
	}
	for(int k = 0; k < n - 2 * i - 2 ; k++)
	{
		printf(" ");
	}
	for(int m = 0; m <= i; m++)
	{
		printf("*");
	}
	printf("\n");
}

for(int i = 0; i < n; i++)
{
	printf("*");
}
	printf("\n");
for(int i = n/2; i > 0 ; i--)
{
	for(int j = 0; j < i; j++)
	{
		printf("*");
	}
	
	for(int k = 0; k < 1 + 2 * (n/2 - i) ;  k++)
	{
		printf(" ");
	}
	
	for(int m = 0; m < i; m++)
	{
		printf("*");
	}
	printf("\n");
		
}

}

else
{
	for(int i = 0; i < n/2 - 1; i++)
{
	for(int j = 0; j <= i; j++)
	{
		printf("*");
	}
	for(int k = 0; k < n - 2 * i - 3 ; k++)
	{
		printf(" ");
	}
	for(int m = 0; m <= i; m++)
	{
		printf("*");
	}
	printf("\n");
}

for(int p = 0; p <= 1; p++)
{
	for(int i = 0; i < n - 1; i++)
	{
		printf("*");
	}
		printf("\n");
}

for(int i = n/2 - 1; i > 0 ; i--)
{
	for(int j = 0; j < i; j++)
	{
		printf("*");
	}
	
	for(int k = 0; k < 1 + 2 * (n/2 - 1 - i) ;  k++)
	{
		printf(" ");
	}
	
	for(int m = 0; m < i; m++)
	{
		printf("*");
	}
	printf("\n");
		
}

}

return 0;

}

*/

/*****************
속이 빈 다이아몬드
*****************/

#include <stdio.h>

int main(void)
{
	int n;
	
	scanf("%d", &n);
	
	if(n % 2 == 1) // 홀수일   
	{
		
	for(int j = 0; j < n/2 ; j++ )
	{
	
			{
				printf(" ");
			}
	}
	
			printf("*");
			
			printf("\n");
	
		for(int i = 0; i < n/2 ; i++)
		{
			for(int j = 1; j < n/2 - i; j++ )
			{
				printf(" ");
			}
			
			printf("*");
			
			for(int k = 0; k< 2 * i + 1; k++)
			{
				printf(" ");
			}
			
			printf("*");
			
			printf("\n");				
		}
		
		for(int i = 0; i < n / 2 - 1; i++)
		{
			for(int j = 0; j <= i; j++ )
			{
				printf(" ");
			}
			
			printf("*");
			
			for(int k = n - 3; k > 2 * i + 1; k--)
			{
				printf(" ");
			}
			
			printf("*");
			
			printf("\n");				
		}
		
	for(int j = 0; j < n/2 ; j++ )
	{
	
			{
				printf(" ");
			}
	}
	
			printf("*");	
		
	}
	
	if(n % 2 == 0)  //짝수일 떄 
	{
		
	for(int j = 0; j < n/2 - 1 ; j++ )
	{
	
			{
				printf(" ");
			}
	}
	
			printf("*");
			
			printf("\n");
	
		for(int i = 0; i < n/2 - 1 ; i++)
		{
			for(int j = 1; j < n/2 - 1 - i; j++ )
			{
				printf(" ");
			}
			
			printf("*");
			
			for(int k = 0; k< 2 * i + 1; k++)
			{
				printf(" ");
			}
			
			printf("*");
			
			printf("\n");				
		}
		
		for(int i = 0; i < n / 2 - 1; i++)
		{
			for(int j = 0; j < i; j++ )
			{
				printf(" ");
			}
			
			printf("*");
			
			for(int k = n - 3; k > 2 * i ; k--)
			{
				printf(" ");
			}
			
			printf("*");
			
			printf("\n");				
		}
		
	for(int j = 0; j < n / 2 - 1 ; j++ )
	{
	
			{
				printf(" ");
			}
	}
	
			printf("*");	
		
	}
	
	return 0;
	
} 
 

	 


