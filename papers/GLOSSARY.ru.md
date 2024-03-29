Глоссарий
===
> Вначале нужно придумать множество слов, одинаковых по смыслу, а затем находить
> различие между ними.

> Надо попытаться сделать описание без слова сущность.

* Пользователь -- User -- персона, зарегистрированная в системе.
* Команда -- Team -- несколько пользователей. У команды есть капитан.
* Участник -- Participant -- пользователь или команда, принимающая участие в
  соревновании.
* Задача  -- Problem -- сущность содержащая в себе текстовое описание для
  человека, а так же специальное описание того, как её проверять (см. набор
  тестов, чекеры, судьи).
* Проблемсет -- Problemset -- упорядоченный набор из нескольких задач.
* Контест -- Contest -- мероприятие, проводимое по определенному проблемсету в
  определенное время по определенным правилам.
* Виртуальный контест -- Virtual Contest -- мероприятие, проводимое для
  одного участника по его собственному запросу так, как будто он принимает
  участие в настоящем контесте, который уже прошел.
* Состояние контеста -- Contest's state -- enum'ное состояние, например
  подготавливается, идёт, закончен (pending, running, over).
* Правила контеста -- Contest's rules -- свод правил, которые определяют
  процедуру регистрации, возможность сдачи задач, изменения в турнирной таблице.


* Роль -- Role -- сущность системы контроля доступа. Роль отвечает на вопрос
  "Кто?" в ACL. Каждый пользователь имеет сопряженную с ним роль. Аналогично
  каждая команда имеет роль. Роль может включать другие роли в себя.
* Группа -- Group -- тоже самое что и роль.
* Ресурс -- Resource -- отвечает на вопрос "Что?" в ACL. Примером ресурса может
  быть задача, контест.
* Действие -- Action -- у ресурса может быть несколько действий, которые над ним
  совершаются, например у задачи может быть два действия: прочитать условие и
  изменить условие.
* Вердикт -- Verdict -- разрешение или не разрешение выполнять некоторое
  действие.
* ACL-Ответ -- ACL-Answer -- это либо вердикт, либо отсутствие вердикта.
* Прецедент -- Precedent -- элемент ACL, говорящий, что некоторой роли
  соответствует вердикт, разрешающий или запрещающий выполнять некоторое
  действие над конкретным ресурсом.
* Политика вывода вердикта -- Verdict decision policy -- описание процедуры
  получения вердикта на основе набора прецедентов, роли и действия. Можно
  описать политику например так:
    1. Если существует среди набора прецедентов ресурса прецедент соответствующей
      текущей роли, то результирующим вердиктом будет вердикт из этого
      прецедента.
    2. Для всех ролей, в которые входит данная роль выполнить эту процедуру, а
      полученный результат свести следующим образом: если есть запрещающий
      вердикт, то ответ "вердикт запрещающий"; в противном случае если
      есть разрешающий вердикт, то ответ "вердикт разрещающий"; в
      остальных случаях ответ "нет вердикта".
  После выволнения этих правил, если получается "нет вердикта", то
  результирующим вердиктом будет запрещающий, иначе будет возвращен тот вердикт,
  который найден.
* Составной ресурс -- Compound Resource -- абстракция над системой acl. Пример:
  есть "ресурс 1" и "ресурс 2", производится запрос на некоторое действие
  -- "прочитать" -- для определенной роли "Пользователь 2", тогда вывод вердикта
  пройдет следующим образом: выводятся вердикты для ресурсов, потом происходит
  свёртка аналогичным с пунктом 2 из политики вывода вердикта.


vim: ts=4 sw=4 et ft=markdown tw=80
