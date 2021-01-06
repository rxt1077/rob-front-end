module Page.About exposing (..)

import Browser
import Html exposing (Html, a, div, h3, h4, h5, p, text)
import Html.Attributes exposing (class)
import Markdown



-- MODEL


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW
-- provides HTML for the main view


view : Model -> Html Msg
view model =
    Markdown.toHtml [ class "content" ] """
# About

> "Can we fix it? Nope, but we can tell you it's broken."
>
> -- _Rob the Builder_

Rob the Builder is a _simple_ testing framework for group projects. It was
designed to help students see the results of running instructor-created
test scripts on their projects.

# Student Quick Start

Student options can all be accessed from the "Student View" module (accessible
from the Navbar at the top of your screen).

# Instructor Quick Start

Rob the Builder is comprised of several objects each of which can be managed via
modules accesible from the Navbar at the top of the screen:

## Jobs

Jobs are used to run Tests on the git repository of a particular Group.
Instructors have access to a listing of all Jobs on the Jobs page. Jobs
that are not currently RUNNING can be deleted. An instructor can also
create a new Job for any Group and Test on the bottom of page.

## Tests

## Groups

## Example

Let's take a look at a C (the language, not the grade) project named
[getenv-wrap](https://github.com/mjmeehan/getenv-wrap). We will create a
script that clones the git repo, compiles the code, and runs a few tests. Then
we will create a _Group_ that has the git repo and the group members, a _Test_
that has the information needed to run the script, and a _Job_ to actually
perform the _Test_

The [documentation for _Rob Run_]
(https://github.com/rxt1077/rob-run/blob/main/README.adoc)
details how to create scripts and build the script ISO. We will be using
`getenv-wrap.sh` which is included in _Rob Run_ as an example.

The first step is to create a _Group_ click on `Groups` in the navbar and `Add
Group` at the bottom of the page. Enter the following:

* Name: getenv-wrap
* Git URL: https://github.com/mjmeehan/getenv-wrap.git

At this point we will not add any members, but if you wanted students to be
able to run/view _Jobs_ you could begin typing their name in `Add Member` and
click on their name in the drop down. You could delete members by clicking the
red garbage icon next to their name.

Next we will create a _Test_ by clicking on `Tests` in the navbar. Click on `Add
test` at the bottom of the page and enter the following:

* Name: getenv-wrap
* Description: Clones, compiles, and tests getenv-wrap
* Base Image: images/debian-10.qcow2
* Tests ISO: tests/tests.iso
* Memory Snapshot: images/memsnapshot.gz
* Prompt: root@rob-run:~#
* Command: /mnt/getenv-wrap.sh
* Timeout (seconds): 600

The `tests.iso` image is mounted in `/mnt` by default, so that is where you
will typically find scripts.

Finally, we need to make a _Job_ to run our _Test_. Click on `Jobs` in the
navbar. Select the `getenv-wrap` _Group_ and `getenv-wrap` _Test_ at the bottom
of the page and click `Add Job`. You should see the _Job_ show up as QUEUED.
Eventually _Rob Run_ should pick up the run the _Job_, changing the status to
RUNNING. Once the _Job_ is complete, you can view the details to see the
output.

"""
