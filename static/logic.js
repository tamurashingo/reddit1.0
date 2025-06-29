function login() {
    var user = document.getElementById('loguser').value;
    var pass = document.getElementById('logpass').value;
    var mem = document.getElementById('logmem');

    var logerror = document.getElementById('logerror');

    if (user === null || user === '') {
        logerror.innerHTML = 'input username';
        return false;
    }

    if (pass === null || pass === '') {
        logerror.innerHTML = 'input password';
        return false;
    }

    var form = document.getElementById('logform');


    new Ajax.Request('/aop', {
        method: 'post',
        parameters: "action=login&" + Form.serialize(form),
        onComplete: (r) => {
            if (r.status === 200 && r.responseText != 'invalid') {
                window.location.reload();
            } else {
                logerror.innerHTML = 'login error';
                form.reset();
            }
        },
    });

    return false;
}

function register() {
    var user = document.getElementById('loguser').value;
    var pass = document.getElementById('logpass').value;

    var logerror = document.getElementById('logerror');

    if (user === null || user === '') {
        logerror.innerHTML = 'input username';
        return false;
    }

    if (pass === null || pass === '') {
        logerror.innerHTML = 'input password';
        return false;
    }

    var form = document.getElementById('logform');


    new Ajax.Request('/aop', {
        method: 'post',
        parameters: "action=register&" + Form.serialize(form),
        onComplete: (r) => {
            if (r.status === 200) {
                window.location.reload();
            } else {
                logerror.innerHTML = 'registration error';
            }
        },
    });

    return false;

}


function logout() {
    new Ajax.Request('/logout', {
        method: 'post',
        onComplete: (r) => {
            window.location.reload();
        }
    });

    return false;
}
