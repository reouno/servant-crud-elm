# servant-crud-elm

# Build

```
stack build
```

# Generate Elm code

```
stack runghc src/GenrateElmApi.hs
```

`elm-dir/DDatumApi.elm` will be exported.

# API

```
# get entity list
curl -XGET 127.0.0.1:8081

# new entity (not implemented)
curl -XPOST 127.0.0.1:8081 \
-H 'Content-type: application/json' \
-H 'Accept: application/json' \
-d '{"datumCreatedAt":"2345-11-11T07:55:02Z","datumContent":"content of the datum.","datumType":"TText","datumName":"title or name of the datum"}'

# get one entity
curl -XGET 127.0.0.1:8081/1

# update entity (not implemented)
curl -XPOST 127.0.0.1:8081/1 \
-H 'Content-type: application/json' \
-H 'Accept: application/json' \
-d '{"datumCreatedAt":"2345-11-11T07:55:02Z","datumContent":"content of the datum.","datumType":"TText","datumName":"title or name of the datum"}'

# delete entity (not implemented)
curl -XDELETE 127.0.0.1:8081/1
```
