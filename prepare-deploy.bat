elm make src/Main.elm --optimize --output=deploy\main.js
rmdir /s /q deploy\css
xcopy /i src\css deploy\css
