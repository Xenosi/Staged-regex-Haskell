#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/resource.h>
#include <unistd.h>

#define LENGTH 1024

double diffUserTime(struct rusage *start, struct rusage *end)
{
    return (end->ru_utime.tv_sec - start->ru_utime.tv_sec) +
           1e-6*(end->ru_utime.tv_usec - start->ru_utime.tv_usec);
}

double diffSystemTime(struct rusage *start, struct rusage *end)
{
    return (end->ru_stime.tv_sec - start->ru_stime.tv_sec) +
           1e-6*(end->ru_stime.tv_usec - start->ru_stime.tv_usec);
}

void main() {
    puts("Starting main");
    char line[LENGTH];

    
    FILE *file = fopen("gentest1.txt", "rt");

    puts("opened file");

    regex_t regex;

    int val;

    val = regcomp(&regex, "^(r)+(f)(U)*(b)(h)+(j)(t)+", 0);

    if (val) {
        puts("Regex did not compile");
        exit(1);
    }

    for (int i=0; i<3;i++) {
        fgets(line, LENGTH, file);
        puts(line);
    }

    puts("Starting reading strings");
    struct rusage start, end;
    for (int i=0;i<10;i++) {
        for (int j=0; j<10; j++) {
            fgets(line,LENGTH,file);
            getrusage(RUSAGE_SELF,&start);
            for (int k=0; k<100; k++) {
                regexec(&regex, line, 0, NULL, 0);
                
                //printf("start:\nsec: %ld\nµsec: %ld\n",start.ru_stime.tv_sec, start.ru_stime.tv_usec);
                //printf("end:\nsec: %ld\nµsec: %ld\n",end.ru_stime.tv_sec, end.ru_stime.tv_usec);
                //printf("%f",time);
            }
            getrusage(RUSAGE_SELF, &end);
            double time = diffSystemTime(&start, &end);
            printf("benchmarking c/%s \ntime    %e \n\n", line, time/100);
        }
        fgets(line,LENGTH,file);
    }

    fclose(file);
} 