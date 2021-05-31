import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.io.*;

public class MatchRegex {

    public static void main(String args[]) {
        String pattern = "(8|T|9|t|l)+";
        Pattern regex = Pattern.compile(pattern);

        try {
            File file = new File("gentest1.txt");
            FileReader fr = new FileReader(file);
            BufferedReader br = new BufferedReader(fr);

            // get rid of first lines

            for (int s=0;s<113;s++) {
                br.readLine();
            }

            String[] lines = new String[100];

            for (int i=0;i<10;i++) {
                br.readLine();
                for (int j=0;j<10;j++) {
                    lines[i*10 + j] = br.readLine();
                }
            }

            fr.close();

            for (String line : lines) {
                System.out.println("benchmarking java/" + line);
                long sum = 0;

                for (int i=0;i<10;i++) {
                    long start = System.nanoTime();
                    Matcher m = regex.matcher(line);
                    if (!m.find()) System.out.println("No match found.");
                    long end = System.nanoTime();
                    sum += end - start;
                } 

                long avg = sum / 10;

                System.out.println("time    " + avg + " ns\n");
            } 
        } catch (IOException e) {
            System.out.println("File exception");
            e.printStackTrace();
        }
        
    } 
} 