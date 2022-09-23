package com.example;

import java.util.ArrayList;

public class Lab1 {

    public static ArrayList<String> singleton(String s) {
        ArrayList<String> arrayList = new ArrayList<>();
        arrayList.add(s);
        return arrayList;
    }

    public static boolean Null(ArrayList<String> arrayList) {
        return arrayList.size() == 0;
    }

    public static ArrayList<String> snoc(ArrayList<String> arr, String s) {
        ArrayList<String> newArray = new ArrayList<>(arr);
        newArray.add(s);
        return newArray;
    }

    public static int length(ArrayList<String> arr) {
        int count = 0;
        for (String i : arr) {
            ++count;
        }
        return count;
    }

    public static void main(String[] args) {
        ArrayList<String> list = new ArrayList<>();
        list.add("cat");
        list.add("dog");

        System.out.println(singleton("tswrawwta"));
        System.out.println(Null(list));

        ArrayList<String> againList = snoc(list, "dino");
        System.out.println(againList);
        System.out.println(length(againList));
    }
}