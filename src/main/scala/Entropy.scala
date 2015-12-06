package com

import java.util.concurrent.ThreadLocalRandom

package object Entropy {
    def rand = ThreadLocalRandom.current()
}
