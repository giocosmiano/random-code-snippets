package com.giocosmiano.exploration.clr;

import com.giocosmiano.exploration.domain.Book;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormatter;
import org.joda.time.format.DateTimeFormatterBuilder;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.stereotype.Component;
import org.springframework.util.ResourceUtils;

import java.io.File;
import java.io.FileReader;
import java.io.Reader;
import java.util.*;

// NOTE: Un-comment if its going to be used for simulation during development. In addition, comment `test` in `de.flapdoodle.embed.mongo` artifact (see POM)
//@Component
public class InitDatabase {

    private static final Logger log = LoggerFactory.getLogger(InitDatabase.class);

    /*
     Pre-loading our MongoDB data store. For such operations, it's recommended to actually use the blocking API.
     That's because when launching an application, there is a certain risk of a thread lock issue when both the
     web container as well as our hand-written loader are starting up. Since Spring Boot also creates a
     MongoOperations object, we can simply grab hold of that
     */

    // NOTE: Un-comment if its going to be used for simulation during development. In addition, comment `test` in `de.flapdoodle.embed.mongo` artifact (see POM)
//    @Bean
    CommandLineRunner init(MongoOperations operations) {
        return args -> {
            initBooks(operations);
        };
    }

    private void initBooks(final MongoOperations operations) {
        try {
            // https://howtodoinjava.com/java/io/read-file-from-resources-folder/
            File file = ResourceUtils.getFile("classpath:sampleJsonData/books.json");
            Reader reader = new FileReader(file);

            // https://stackoverflow.com/questions/10926353/how-to-read-json-file-into-java-with-simple-json-library
            // https://howtodoinjava.com/library/json-simple-read-write-json-examples/
            operations.dropCollection(Book.class);

            JSONParser parser = new JSONParser();
            Object obj = parser.parse(reader);

            JSONArray listOfBooks = (JSONArray) obj;
            listOfBooks.forEach(bookObj -> {
                JSONObject book = (JSONObject) bookObj;

                // id & pageCount
                Long id = (Long) book.get("_id");
                Long pageCount = (Long) book.get("pageCount");

                // published date
                // https://stackoverflow.com/questions/15333320/how-to-convert-joda-time-datetime-to-java-util-date-and-vice-versa
                // https://stackoverflow.com/questions/23987332/joda-datetime-isodatetimeformat-pattern
                String publishedDateStr = null;
                Date publishedDate = null;
                JSONObject publishedDateObj = (JSONObject) book.get("publishedDate");
                if (Objects.nonNull(publishedDateObj)) {
                    publishedDateStr = (String) publishedDateObj.get("$date");
                    DateTimeFormatter patternFormat = new DateTimeFormatterBuilder()
                            .appendPattern("yyyy-MM-dd'T'HH:mm:ss.SSS")
                            .appendTimeZoneOffset("Z", true, 2, 4)
                            .toFormatter();
                    DateTime publishedDateInJoda = DateTime.parse(publishedDateStr, patternFormat);
                    publishedDate = publishedDateInJoda.toDate();
                }

                // authors
                List<String> authors = new ArrayList<>();
                JSONArray authorsObj = (JSONArray) book.get("authors");
                authorsObj.forEach(author -> authors.add((String)author));

                // categories
                List<String> categories = new ArrayList<>();
                JSONArray categoriesObj = (JSONArray) book.get("categories");
                categoriesObj.forEach(category -> categories.add((String)category));

                Book newBook = new Book(
                        id
                        , (String) book.get("title")
                        , (String) book.get("isbn")
                        , pageCount
                        , publishedDate
                        , (String) book.get("thumbnailUrl")
                        , (String) book.get("shortDescription")
                        , (String) book.get("longDescription")
                        , (String) book.get("status")
                        , authors
                        , categories
                );

                operations.insert(newBook);
                log.debug(String.format("Inserted Book %s", newBook));
            });

            reader.close();
            log.info(String.format("Finished loading %s books", listOfBooks.size()));

        } catch (Exception e) {
            log.error(e.getMessage());
        }
    }
}