import java.io.*;

public class ReadClass {

    public static void main(String... args) throws Exception {

        File file = new File("HFunction.class");
        int size = (int) file.length();
                
        FileInputStream stream = new FileInputStream(file);
        PrintStream out = new PrintStream("src/hfunction.h");

        byte[] bytes = new byte[size];

        stream.read(bytes);

        out.printf("\n#define FFIJNI_HFUNCTION_LENGTH %d\n\n", size);
        out.print("static jbyte hFunctionClass[FFIJNI_HFUNCTION_LENGTH] = {");
        for (int i = 0; i < size; i++) {
            if (i % 16 == 0) {
                out.println();
                out.print("  ");
            }
            out.print(bytes[i]);
            if (i+1 < size) {
                out.print(", ");
            }
        }
        out.println("\n};");
    }
}


