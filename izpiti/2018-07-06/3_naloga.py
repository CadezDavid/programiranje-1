# 3. naloga

def simetricen(string):
    dolzina = len(string)
    if dolzina <= 1:
        return True
    elif string[0] == string[dolzina - 1]:
        return simetricen(string[1:(dolzina - 1)])
    else:
        return False


def stevilo_delov(string):
    if simetricen(string):
        return 1
    else:
        return 1 + min(
                [stevilo_delov(string[i:]) for i in range(1, len(string))
                    if simetricen(string[0:i])]
        )


def vsotno_simetricen(string):
    seznam = [int(c) for c in string]
    return sum(seznam[0:len(seznam)//2]) == sum(seznam[len(seznam)//2:])


def stevilo_delov_2(string, simetrija):
    if simetrija(string):
        return 1
    else:
        return 1 + min(
                [stevilo_delov(string[i:]) for i in range(1, len(string))
                    if simetrija(string[0:i])]
        )


def razdeli(string, simetrija):
    if simetrija(string):
        return [string]
    else:
        current = len(string) + 1
        for i in range(len(string)):
            if (stevilo_delov_2(string[i:], simetrija) < current
                    and simetrija(string[0:i])):
                new = string[0:i]
                dolzina = i
                current = stevilo_delov_2(string[i:], simetrija)
        return [new] + razdeli(string[dolzina:], simetrija)
