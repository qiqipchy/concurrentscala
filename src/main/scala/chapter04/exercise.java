package chapter04;

interface exewe {
    int i = 0;
}
class mm implements exewe {
    int a = 3;
}
public class exercise {
    public static void main(String[] args) {
        System.out.println(new mm().i);
    }
}