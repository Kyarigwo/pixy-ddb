# pixy-ddb

- Add type safety?
- Add validation/compilation phase.
- Measure proformance.
- Improve proformance.
- Add comparison predicates.
- Add negative atoms.
- Add aggregate functions.
- Add uninterprated functions.
- Store database to file?

## Application

As an example/development template, use a basic chat application.  Will update
definition as we change it.

- event:: Predicate [(event, Event), (at, Datetime)]
- chat:: Predicate [(chat, Chat), (name, Chatname)]
- participant:: Predicate [(participant, Participant), (name, ParticipantName)]
- enter: Predicate [(event, Event), (chat, Chat), (participant, Participant)]
- message: Predicate [(event, Event),
                    (chat, Chat), (participant, Participant),
                    (message, Message)]
- exit: Predicate [(event, Event), (chat, Chat), (participant, Participant)]

Participants in chat "This chat":

- query(name) <== chat(chat, name = "This chat") /\ enter(chat, participant)
                  /\ participant(participant, name).
