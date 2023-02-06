use gloo_worker::Registrable;
use web::eval::PseudocodeEvaluator;

fn main() {
    PseudocodeEvaluator::registrar().register();
}
