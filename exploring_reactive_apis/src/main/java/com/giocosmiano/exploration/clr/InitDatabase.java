package com.giocosmiano.exploration.clr;

import com.giocosmiano.exploration.domain.Book;
import com.giocosmiano.exploration.domain.H2Book;
import com.giocosmiano.exploration.repository.H2BookRepository;
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

// NOTE:
// 1) Using embedded MongoDB with flapdoodle for Reactive Mongo simulation
// 2) Using H2 DB to simulate DB access with Reactor Flux/Mono for non-blocking IO
// See `readme.md` about the 2 ways of simulating Reactive Mongo using installed MongoDB vs embedded MongoDB with flapdoodle
@Component
public class InitDatabase {

    private static final Logger log = LoggerFactory.getLogger(InitDatabase.class);

    /*
     Pre-loading our MongoDB data store. For such operations, it's recommended to actually use the blocking API.
     That's because when launching an application, there is a certain risk of a thread lock issue when both the
     web container as well as our hand-written loader are starting up. Since Spring Boot also creates a
     MongoOperations object, we can simply grab hold of that
     */

    @Bean
    CommandLineRunner init(
            MongoOperations mongoOperations
            , H2BookRepository h2BookRepository) {
        return args -> {
            initBooks(mongoOperations, h2BookRepository);
        };
    }

    private void initBooks(
            final MongoOperations mongoOperations
            , final H2BookRepository h2BookRepository) {
        try {
            // https://howtodoinjava.com/java/io/read-file-from-resources-folder/
            File file = ResourceUtils.getFile("classpath:sampleJsonData/books.json");
            Reader reader = new FileReader(file);

            // https://stackoverflow.com/questions/10926353/how-to-read-json-file-into-java-with-simple-json-library
            // https://howtodoinjava.com/library/json-simple-read-write-json-examples/
            mongoOperations.dropCollection(Book.class);
            h2BookRepository.deleteAll();

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

                String title = (String) book.get("title");
                String isbn = (String) book.get("isbn");
                String thumbnailUrl = (String) book.get("thumbnailUrl");
                String shortDescription = (String) book.get("shortDescription");
                String longDescription = (String) book.get("longDescription");
                String status = (String) book.get("status");

                Book newBook = new Book(
                        id
                        , title
                        , isbn
                        , pageCount
                        , publishedDate
                        , thumbnailUrl
                        , shortDescription
                        , longDescription
                        , status
                        , authors
                        , categories
                );
                mongoOperations.insert(newBook);

                H2Book h2Book = new H2Book();
                h2Book.setId(id);
                h2Book.setTitle(title);
                h2Book.setIsbn(isbn);
                h2Book.setPageCount(pageCount);
                h2Book.setPublishedDate(publishedDate);
                h2Book.setThumbnailUrl(thumbnailUrl);
                h2Book.setShortDescription(shortDescription);
                h2Book.setLongDescription(longDescription);
                h2Book.setStatus(status);
                h2Book.setAuthors(authors);
                h2Book.setCategories(categories);
                h2BookRepository.save(h2Book);

                log.debug(String.format("Inserted Book %s", newBook));
            });

            reader.close();
            log.info(String.format("Finished loading %s books", listOfBooks.size()));

        } catch (Exception e) {
            log.error(e.getMessage());
        }
    }
}